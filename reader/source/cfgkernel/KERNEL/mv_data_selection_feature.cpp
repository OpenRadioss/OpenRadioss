/*Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2025 Altair Engineering Inc.
//Copyright>
//Copyright>    This program is free software: you can redistribute it and/or modify
//Copyright>    it under the terms of the GNU Affero General Public License as published by
//Copyright>    the Free Software Foundation, either version 3 of the License, or
//Copyright>    (at your option) any later version.
//Copyright>
//Copyright>    This program is distributed in the hope that it will be useful,
//Copyright>    but WITHOUT ANY WARRANTY; without even the implied warranty of
//Copyright>    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//Copyright>    GNU Affero General Public License for more details.
//Copyright>
//Copyright>    You should have received a copy of the GNU Affero General Public License
//Copyright>    along with this program.  If not, see <https://www.gnu.org/licenses/>.
//Copyright>
//Copyright>
//Copyright>    Commercial Alternative: Altair Radioss Software
//Copyright>
//Copyright>    As an alternative to this open-source version, Altair also offers Altair Radioss
//Copyright>    software under a commercial license.  Contact Altair to discuss further if the
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.*/
#include <UTILS/win32_utils.h>  

#include "mv_descriptor.h"
#include "mv_data_selection_feature.h"
#include "mv_test.h"
#include "mv_data_radio_feature.h"
#include "mv_data_data_feature.h"
#include "mv_data_if_feature.h"
#include <KERNEL_BASE/expression_API.h>
typedef map<int,int>        LocTypeIKeywordMap_t;
typedef set<int>            LocTypeValueSet_t;
/* --------- Constructors & destructor --------- */

MvDataSelectionFeature_t::MvDataSelectionFeature_t(const MvDescriptor_t *descr_p,
                                                   int default_ikeyword) :
  MvDataFeature_t(DFT_SELECTION,""),
  p_default_ikeyword(default_ikeyword),
  p_type_ikeyword(END_ARGS),
  myTypeIKeywordMapPtr((MvPseudoTypeIKeywordMap_t *) (new LocTypeIKeywordMap_t()))
{
    //get the type_ikeyword and set the same then call the method to populate the map
    string default_skeyword=descr_p->getSKeyword(default_ikeyword);
    size_t pos = default_skeyword.find("_",0);
    string rootname = default_skeyword.substr(0,pos);
    int type_ikeyword = descr_p->getIKeyword(rootname+"_TYPE");
    p_type_ikeyword=type_ikeyword;
    AddSupportFeatureMap(descr_p);
}

MvDataSelectionFeature_t::~MvDataSelectionFeature_t() {
    delete (LocTypeIKeywordMap_t *)myTypeIKeywordMapPtr;
}

static void loc_add_expression_type_values(const expression_t *expr_p, int type_ikwd,
                                           LocTypeValueSet_t &type_value_set)
{
    expression_type_e expr_type = EXPRT_UNKNOWN;
    MCDS_get_expression_attributes(expr_p, EXPR_TYPE, &expr_type, END_ARGS);
    if(expr_type == EXPRT_ATTRIBUTE)
    {
        //this typevalue becomes the identifier for the map below
        // this number represents the digit part of the syntax in config file
        //if(SECONDARY_TYPE == 0)
        int type_value=-2, expr_ikwd=END_ARGS;
        MCDS_get_expression_attributes(expr_p,
            EXPR_RVALUE,&type_value,EXPR_IKEYWORD,&expr_ikwd,END_ARGS);
        if(expr_ikwd == type_ikwd) type_value_set.insert(type_value);
    } else if(expr_type == EXPRT_LOGICAL) {
        //if in the config file expression is of type if(SECONDARY_TYPE == 1 || SECONDARY_TYPE == 2)
        // or (even more important) if(CARD_EXISTS == TRUE && IPIDXCL_TYPE == 0)
        //this block should handle it properly
        expression_t *expr1_p=NULL, *expr2_p=NULL;
        MCDS_get_expression_attributes(expr_p,EXPR_FIRST_EXPR,&expr1_p,
            EXPR_SECOND_EXPR,&expr2_p,END_ARGS);
        if(expr1_p) loc_add_expression_type_values(expr1_p, type_ikwd, type_value_set);
        if(expr2_p) loc_add_expression_type_values(expr2_p, type_ikwd, type_value_set);
    }
}

static void loc_update_type_ikwd_map(const LocTypeValueSet_t &type_value_set, int ikwd,
                                     LocTypeIKeywordMap_t &type_ikwd_map)
{
    LocTypeValueSet_t::const_iterator set_itr = type_value_set.begin();
    LocTypeValueSet_t::const_iterator set_itr_end = type_value_set.end();
    while(set_itr != set_itr_end)
    {
        int type_value = (*set_itr);
        type_ikwd_map[type_value] = ikwd;
        ++set_itr;
    }
}

static void loc_update_type_ikwd_map(const LocTypeValueSet_t &type_value_set,
                                     const MvDataFeatureList_t &featurelist,
                                     const MvDescriptor_t *descr_p,
                                     const string &rootname,
                                     LocTypeIKeywordMap_t &type_ikwd_map)
{
    MvDataFeatureList_t::const_iterator itr = featurelist.begin();
    MvDataFeatureList_t::const_iterator itr_end = featurelist.end();
    while(itr != itr_end)
    {
        const MvDataFeature_t *feature_p = (const MvDataIfFeature_t *)(*itr);
        if(DFT_SUPPORT == feature_p->getType())
        {
            MvDataSupportFeature_t *supportfeature_p = (MvDataSupportFeature_t *)feature_p;
            int nb_ikeywords = supportfeature_p->getNbAttributes();
            for(int i=0;i<nb_ikeywords;i++)
            {
                int support_ikeyword = supportfeature_p->getAttributeIKeyword(i);
                if(descr_p->getObjectType(support_ikeyword) == HCDI_OBJ_TYPE_SETS)
                {
                    //we have made sure it a support for some group type
                    //we will also make sure that it also contans rootname in skeyword
                    //rootname should always be in the first part of the name
                    string support_skeyword = descr_p->getSKeyword(support_ikeyword);
                    size_t pos = support_skeyword.find(rootname.c_str(),0,rootname.size());
                    if(pos == 0)
                    {
                        loc_update_type_ikwd_map(type_value_set, support_ikeyword,
                            type_ikwd_map);
                    }
                }
            }
        } else if(DFT_DATA == feature_p->getType())
        {
            MvDataDataFeature_t *datafeature_p = (MvDataDataFeature_t *)feature_p;
            int data_ikeyword = datafeature_p->getIKeyword();
            loc_update_type_ikwd_map(type_value_set, data_ikeyword,
                type_ikwd_map);
        }
        ++itr;
    }
}

void MvDataSelectionFeature_t::AddSupportFeatureMap(const MvDescriptor_t *descr_p)
{
    //try to create a new one,update the map & return the same
    if(NULL == myTypeIKeywordMapPtr) return; // shouldn't happen, just to be clean ...
    LocTypeIKeywordMap_t &type_ikwd_map = *((LocTypeIKeywordMap_t *)myTypeIKeywordMapPtr);
    LocTypeValueSet_t type_value_set;

    string a_rootname = descr_p->getSKeyword(p_type_ikeyword);
    size_t pos = a_rootname.find("_");
    a_rootname.erase(pos);

    //find the radio feature corresponding to the given ikeyword
    const MvDataFeature_t *typefeature_p = descr_p->getIkeywordDataFeature(DOM_COMMON,p_type_ikeyword);
    if(NULL == typefeature_p) return; // shouldn't happen, just to be clean ...

    //now find all the "if" features and try to see if radio feature is part of it
    //so this set contians on
    MvDataFeatureSet_t *iffeatures_p = descr_p->getIfFeatures(DOM_COMMON,typefeature_p);
    MvDataFeatureSet_t::const_iterator it = iffeatures_p->begin();
    MvDataFeatureSet_t::const_iterator it_end = iffeatures_p->end();
    MvDataIfFeature_t *iffeature_p=NULL;
    bool found = false;
    while((it!= it_end) && !found)//looping all the "if" until we have found the right one
    {
        iffeature_p = (MvDataIfFeature_t *)(*it);
        int nb_tests = iffeature_p->getNbTests();
        // we are also interested in the number of possibilities including else (cf below)
        int nb_poss = nb_tests;
        const MvDataFeatureList_t &featurelist_else = iffeature_p->getDefaultFeaturePtrList();
        if(featurelist_else.size() > 0) nb_poss++;

        //this means that this if feature is indeed checking the value of rootname_TYPE
        if(1 < nb_poss)
        {
            // We have found a condition with at least 2 possibilites (including else),
            // so suppose this is the right one:
            found = true;
            //now we scan the contents of different if / else if conditions
            for(int i=0;i<nb_tests;i++)
            {
                //getting the expression pointer and get the vlue associated with it
                const MvExpression_t *expr_p = iffeature_p->getExpressionPtr(i);
                const expression_t *expr = expr_p->getExpressionPtr();

                //we get the value against which rootname_TYPE is checked in if conditions
                //we map the same with the ikeywords ant try to find the type
                type_value_set.clear();
                loc_add_expression_type_values(expr, p_type_ikeyword, type_value_set);

                //now that we got the value or values of rootname_TYPE
                //get the ikeyword associated with it(could be DATA or SUPPORT feature)
                const MvDataFeatureList_t &featurelist = iffeature_p->getFeaturePtrList(i);
                loc_update_type_ikwd_map(type_value_set,
                    featurelist, descr_p, a_rootname, type_ikwd_map);
            }

            // if there is an "else" block, we try to find an unused value of the radiofeature,
            // and map this unused value to the feature(s)/ikeyword(s) in the else block.
            if(featurelist_else.size() > 0)
            {
                MvDataFeatureType_e feature_type = typefeature_p->getType();
                if( (DFT_RADIO == feature_type) &&
                    (VTYPE_INT == ((MvDataRadioFeature_t*) typefeature_p)->getValueType()))
                {
                    MvDataRadioFeature_t *radiofeature_p = (MvDataRadioFeature_t*) typefeature_p;
                    int nb_items = radiofeature_p->getNumber();
                    for(int j = 0; j < nb_items; j++)
                    {
                        int type_value = radiofeature_p->getRadioIntValue(j);
                        if(type_ikwd_map.count(type_value) == 0)
                        {
                            // type_value is not yet mapped to any feature/ikeyword, so we map it.
                            type_value_set.clear();
                            type_value_set.insert(type_value);
                            loc_update_type_ikwd_map(type_value_set, 
                                featurelist_else, descr_p, a_rootname, type_ikwd_map);
                            break; // no need to go on
                        }
                    }
                }
                // else: e.g. flag feature might be implemented here
            }
        }
        ++it;
    }
    delete iffeatures_p;
}

int MvDataSelectionFeature_t::getTypeNumber() const
{
    LocTypeIKeywordMap_t *type_iKeyword_map_ptr =  (LocTypeIKeywordMap_t *)myTypeIKeywordMapPtr;
    return (int) type_iKeyword_map_ptr->size();
}

int MvDataSelectionFeature_t::getTypeValue(int i) const
{

    LocTypeIKeywordMap_t *type_iKeyword_map_ptr =  (LocTypeIKeywordMap_t *)myTypeIKeywordMapPtr;
    if(i >= type_iKeyword_map_ptr->size()) return -2;
    int index=0;
    LocTypeIKeywordMap_t::iterator itr=type_iKeyword_map_ptr->begin();
    LocTypeIKeywordMap_t::iterator itr_end=type_iKeyword_map_ptr->end();
    while(itr!=itr_end)
    {
    if(index == i)return (*itr).first;
    else { ++itr;index++;}
    }
    return -1;
}

int MvDataSelectionFeature_t::getTypeAttribute(int type_value) const
{
    LocTypeIKeywordMap_t *type_iKeyword_map_ptr =  (LocTypeIKeywordMap_t *)myTypeIKeywordMapPtr;
    if(NULL == type_iKeyword_map_ptr)                 return END_ARGS;
    if(type_iKeyword_map_ptr->count(type_value) == 0) return END_ARGS;
    return (*type_iKeyword_map_ptr)[type_value];
}

int MvDataSelectionFeature_t::getTypefromIkeyword(int ikeyword) const
{
    LocTypeIKeywordMap_t *type_iKeyword_map_ptr =  (LocTypeIKeywordMap_t *)myTypeIKeywordMapPtr;
    LocTypeIKeywordMap_t::iterator itr=type_iKeyword_map_ptr->begin();
    LocTypeIKeywordMap_t::iterator itr_end=type_iKeyword_map_ptr->end();
    while(itr!=itr_end)
    {
        if((*itr).second == ikeyword && (*itr).first != -1)
            return (*itr).first;
        ++itr;
    }
    return 0;
}

extern "C"
int MV_dataselfeature_get_group_type_ikeyword(const MvPseudoDataSelectionFeature_t *datasel_p)
{
    if(datasel_p==NULL) return -1;
    const MvDataSelectionFeature_t *dataselectionfeature_p = (const MvDataSelectionFeature_t *)datasel_p;
    return dataselectionfeature_p->GetTypeIKeyword();
}


extern "C"
int MV_get_group_ikeyword_for_typevalue(const MvPseudoDataSelectionFeature_t *datasel_p,int typevalue)
{
    if(datasel_p==NULL) return -1;
    const MvDataSelectionFeature_t *dataselectionfeature_p = (const MvDataSelectionFeature_t *)datasel_p;
    return dataselectionfeature_p->getTypeAttribute(typevalue);
}
