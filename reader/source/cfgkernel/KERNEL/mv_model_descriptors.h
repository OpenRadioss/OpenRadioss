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

//

#ifndef MV_MODEL_DESCRIPTORS_H
#define MV_MODEL_DESCRIPTORS_H

#include <UTILS/mv_string.h>

#include "mv_descriptor.h"
#include "mv_control_card_types.h"
#include <HCDI/hcdi.h>

/** @name Model descriptors (control cards) */
//@{

/// Initializing model descriptors
//void MV_init_model_descriptors();
/// Closing model descriptors
///void MV_close_model_descriptors();
/// Getting model i-keyword
int MV_get_model_ikeyword(const string &skeyword);
/// Getting model s-keyword
const string &MV_get_model_skeyword(int ikeyword);

/// Getting a control card's descriptor


//@}
class HC_DATA_DLL_API MvDescriptorMap_t {
public:
    MvDescriptorMap_t() : myPreDescriptors() {
    }
    ~MvDescriptorMap_t();
//public:
//    const MvDescriptor_t* getDescriptorPtr(MvControlCardType_e cc_type) const {
//        MvPreDescriptorMap_t::const_iterator a_it = myPreDescriptors.find(cc_type);
//        if (a_it == myPreDescriptors.end()) return NULL;
//        return (*a_it).second->getDescriptorPtr();
//    }
//private:
//    void addPreDescriptor(void *cfgkernel, const string& file_name, MvControlCardType_e cc_type) {
//        MvPreDescriptor_t* a_pre_descr_p = new MvPreDescriptor_t(file_name);
//        myPreDescriptors[cc_type] = a_pre_descr_p;
//        a_pre_descr_p->getDescriptorPtr();
//    }
private: // Types
    typedef map<MvControlCardType_e, MvPreDescriptor_t*> MvPreDescriptorMap_t;
private: // Data
    MvPreDescriptorMap_t myPreDescriptors;
};

#endif //MV_MODEL_DESCRIPTORS_H




