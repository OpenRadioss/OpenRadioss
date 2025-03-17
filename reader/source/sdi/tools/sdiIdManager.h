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




////////////////////////////////////////////////////////////////////////////////////

#if !defined(SDIIDMANAGER__INCLUDED_)
#define SDIIDMANAGER__INCLUDED_

#include <sdiModelView.h>

#include <map>
#include <vector>

namespace sdi
{

// Class to keep track of the "nextAvailableId" for each include and entity type.
// The idea is that, for each entity type, the includes have their current id-ranges
// so that for all ids in this include the following is true:
// p_offsetsOfIncludes[includeId][type] < id < p_nextAvailableIdsOfIncludes[includeId][type]
// So, for any include and type, a new entity can get the id
// p_nextAvailableIdsOfIncludes[includeId][type]
// For this to work, the following has to be true and kept up to date:
// - All includes with the same offset also have the same nextAvailableId.
//   They have the same id ranges.
// - If the nextAvailableId of an include with a lower offset is greater than the offset of
//   another include with a higher offset, the id ranges of the 2 includes overlap and so they
//   also have to have the same nextAvailableId.
// These two conditions mean that the nextAvailableIds of different includes may have to be
// "kept in sync", or to be "sync'ed".
//
// NB: Id-pools to be done!!!
class SDIIdManager
{
public:
    SDIIdManager(const ModelViewEdit& mv,
                 const std::vector<std::set<EntityType>> idPools = {{}})
        : p_mv(mv)
    {
        // create empty arrays for the main file
        p_nextAvailableIdsOfIncludes[0].resize(p_mv.GetMaxEntityType() + 1, 1);
        p_offsetsOfIncludes[0].resize(p_mv.GetMaxEntityType() + 1, 0);
        // p_maxIdsOfIncludes[0].resize(p_mv.GetMaxEntityType() + 1, 0);
        p_pNextAvailableIds = &(p_nextAvailableIdsOfIncludes[0]);
        p_pOffsets = &(p_offsetsOfIncludes[0]);
        // create idpool tables
        p_idPools.resize(p_mv.GetMaxEntityType() + 1);
        for(auto idPool : idPools)
        {
            for(EntityType type : idPool)
            {
                for(EntityType otherTypeInIdPool : idPool)
                {
                    if(otherTypeInIdPool != type) p_idPools[type].insert(otherTypeInIdPool);
                }
            }
        }
    }

    virtual ~SDIIdManager() {}

    void SetCurrentInclude(unsigned int includeId)
    {
        if(p_nextAvailableIdsOfIncludes.count(includeId) == 0)
        {
            InitOffsets(includeId);
            SyncNextAvailableIds();
        }
        p_pNextAvailableIds = &(p_nextAvailableIdsOfIncludes[includeId]);
        p_pOffsets = &(p_offsetsOfIncludes[includeId]);
    }

    unsigned int GetNextAvailableId(EntityType etype)
    {
        assert(nullptr != p_pNextAvailableIds);
        assert(p_pNextAvailableIds->size() > etype);
        return p_pNextAvailableIds->at(etype);
    }

    void AddId(unsigned int id, EntityType etype)
    {
        assert(nullptr != p_pNextAvailableIds);
        assert(p_pNextAvailableIds->size() > etype);
        if(id >= p_pNextAvailableIds->at(etype))
        {
            ReplaceNextAvailableId(id + 1, etype,
                p_pNextAvailableIds->at(etype), p_pOffsets->at(etype));
        }
    }

    void InitOffsets()
    {
        for(auto it = p_offsetsOfIncludes.begin(); it != p_offsetsOfIncludes.end(); ++it)
        {
            InitOffsets(it->first);
        }
        SyncNextAvailableIds();
    }

    // Init from a non-empty model
    virtual void Init() {}

protected:
    void Init(const sdiString& includeKeyword)
    {
        // init offsets
        SelectionRead selIncludes(&p_mv, includeKeyword);
        while(selIncludes.Next())
        {
            InitOffsets(selIncludes->GetId(), true);
        }

        // init nextAvailableIds by looping through all entities
        EntityType maxType = p_mv.GetMaxEntityType();
        for(EntityType type = ENTITY_TYPE_NONE + 1; type <= maxType; ++type)
        {
            const sdiString& keyword = p_mv.GetKeyword(type);
            if(keyword.size() == 0) continue;
            if(keyword == includeKeyword) continue;
            SelectionRead sel(&p_mv, keyword);
            while(sel.Next())
            {
                assert(sel->GetType() == type);
                unsigned int selId = sel->GetId();
                HandleRead hInclude = sel->GetInclude();
                unsigned int includeId = hInclude.GetId(&p_mv);
                assert(p_nextAvailableIdsOfIncludes.count(includeId) > 0);
                assert(p_offsetsOfIncludes.count(includeId) > 0);
                
                assert((int) selId >= p_offsetsOfIncludes[includeId][type] || 0 == selId || sel->GetKeyword() == "/SET/COLLECT");
                std::vector<unsigned int>& nextAvailableIds = p_nextAvailableIdsOfIncludes[includeId];
                if(selId >= nextAvailableIds[type]) nextAvailableIds[type] = selId + 1;
            }
        }

        SyncNextAvailableIds();
    }

private:
    void InitOffsets(unsigned int includeId, bool doReset = false)
    {
        EntityType maxType = p_mv.GetMaxEntityType();
        if(p_nextAvailableIdsOfIncludes.count(includeId) == 0)
        {
            p_nextAvailableIdsOfIncludes[includeId].resize(maxType + 1, 1);
            p_offsetsOfIncludes[includeId].resize(maxType + 1, 0);
            // p_maxIdsOfIncludes[includeId].resize(maxType + 1, 0);
        }
        std::vector<int>& offsets = p_offsetsOfIncludes[includeId];
        GetOffsets(includeId, offsets);
        std::vector<unsigned int>& nextAvailableIds = p_nextAvailableIdsOfIncludes[includeId];
        for(EntityType type = ENTITY_TYPE_NONE + 1; type <= maxType; ++type)
        {
            if(doReset) nextAvailableIds[type] = 1;
            if(0 < offsets[type] && nextAvailableIds[type] <= (unsigned int) offsets[type])
            {
                nextAvailableIds[type] = offsets[type] + 1;
            }
        }
    }

    void SyncNextAvailableIds()
    {
        EntityType maxType = p_mv.GetMaxEntityType();
        for(EntityType etype = ENTITY_TYPE_NONE + 1; etype <= maxType; ++etype)
        {
            SyncNextAvailableIds(etype);
        }
    }

    void SyncNextAvailableIds(EntityType etype)
    {
        // first find the highest nextAvailableId for each offset
        std::map<int, unsigned int> offsetVsNextAvailableId;
        std::map<unsigned int, std::vector<unsigned int>>::iterator it;
        for(it = p_nextAvailableIdsOfIncludes.begin();
            it != p_nextAvailableIdsOfIncludes.end(); ++it)
        {
            unsigned int nextAvailableId = it->second[etype];
            int offset = p_offsetsOfIncludes[it->first][etype];
            if(offsetVsNextAvailableId.count(offset) == 0)
            {
                offsetVsNextAvailableId[offset] = nextAvailableId;
            }
            else if(offsetVsNextAvailableId[offset] < nextAvailableId)
            {
                offsetVsNextAvailableId[offset] = nextAvailableId;
            }
        }

        // then "sync" the nextAvailableId for the offsets
        if(offsetVsNextAvailableId.size() > 1)
        {
            std::map<int, unsigned int>::iterator itOffsetHi = offsetVsNextAvailableId.end();
            std::map<int, unsigned int>::iterator itOffsetLo = itOffsetHi;
            --itOffsetLo;
            do
            {
                --itOffsetHi;
                --itOffsetLo;
                if(itOffsetLo->second > itOffsetHi->second)
                {
                    // the nextAvailableId of the include(s) with lower offset has become greater
                    // than the nextAvailableId of the include(s) with higher offset.
                    // This means the nextAailableId of the include(s) with higher offset has to be
                    // synced, and maybe even includes with still higher offsets
                    do
                    {
                        itOffsetHi->second = itOffsetLo->second;
                        ++itOffsetHi;
                        ++itOffsetLo;
                    } while(itOffsetHi != offsetVsNextAvailableId.end() &&
                            itOffsetLo->second > itOffsetHi->second);
                }
                else if((int) itOffsetLo->second > itOffsetHi->first)
                {
                    // the includes have overlapping range, so the lower one has to be synced
                    // to the higher one
                    itOffsetLo->second = itOffsetHi->second;
                }
            } while(itOffsetLo != offsetVsNextAvailableId.begin());
        }

        // now propagate the new nextAvailableIds to p_nextAvailableIdsOfIncludes
        for(it = p_nextAvailableIdsOfIncludes.begin();
            it != p_nextAvailableIdsOfIncludes.end(); ++it)
        {
            unsigned int nextAvailableId = it->second[etype];
            int offset = p_offsetsOfIncludes[it->first][etype];
            assert(it->second[etype] <= offsetVsNextAvailableId[offset]);
            it->second[etype] = offsetVsNextAvailableId[offset];
            // ... also propagate to other types in the same idpool
            for(EntityType otherTypeInIdPool : p_idPools[etype])
            {
                if(it->second[otherTypeInIdPool] < it->second[etype])
                {
                    it->second[otherTypeInIdPool] = it->second[etype];
                }
                // sanity check here: all types of an id pool must have the same offset:
                assert(p_offsetsOfIncludes[it->first][etype] == 
                       p_offsetsOfIncludes[it->first][otherTypeInIdPool]);
            }
        }
    }

    void ReplaceNextAvailableId(unsigned int newNextAvailableId, EntityType etype,
        unsigned int oldNextAvailableId, int offset, bool doRecursive = true)
    {
        bool syncIsNeeded = false;

        for(std::map<unsigned int, std::vector<unsigned int>>::iterator it = p_nextAvailableIdsOfIncludes.begin();
            it != p_nextAvailableIdsOfIncludes.end(); ++it)
        {
            std::vector<unsigned int>& nextAvailableIds = it->second;
            assert(nextAvailableIds.size() > etype);
            if(nextAvailableIds[etype] == oldNextAvailableId)
            {
                // this may be the include in which a new id is just being set, or another one
                // which has its nextAvailableId "sync'ed"
                // First set for the requested type...
                nextAvailableIds[etype] = newNextAvailableId;
                // ... then for other types in the idpool, if there are
                for(EntityType otherTypeInIdPool : p_idPools[etype])
                {
                    nextAvailableIds[otherTypeInIdPool] = newNextAvailableId;
                }
            }
            else
            {
                unsigned int includeId = it->first;
                assert(p_offsetsOfIncludes.count(includeId) > 0);
                std::vector<int>& offsets = p_offsetsOfIncludes[includeId];

                if( offsets[etype] > offset && // it's an include with a higher offset than the active one ...
                    offsets[etype] < (int) newNextAvailableId) // ... but the id-ranges overlap
                {
                    syncIsNeeded = true;
                    break;
                }
            }
        }

        if(syncIsNeeded)
        {
            // make sure at least this one is set, in case the loop hadn't reached it:
            p_pNextAvailableIds->at(etype) = newNextAvailableId;
            // now run a sync from scratch
            SyncNextAvailableIds(etype);
        }
    }

    // has to be redefined by application, otherwise not usable
    virtual void GetOffsets(unsigned int includeId, std::vector<int>& offsets)
    {}

protected:
    const ModelViewEdit& p_mv;

private:
    // In the following data members:
    // The key in the outer map is the parentSubmodelFileID,
    // the index in the array is the entityType
    std::map<unsigned int, std::vector<unsigned int>> p_nextAvailableIdsOfIncludes;
    std::map<unsigned int, std::vector<int>> p_offsetsOfIncludes;
    // std::map<unsigned int, std::vector<unsigned int>> p_maxIdsOfIncludes;
    // ... and for convenience a pointer to the ones of the current include
    std::vector<unsigned int>* p_pNextAvailableIds = nullptr;
    std::vector<int>* p_pOffsets = nullptr;
    // Id pool information: For each entity type (outer vector) we store the other
    // entity types of the id pool.
    std::vector<std::set<EntityType>> p_idPools;
};

} // namespace sdi

#endif //! !defined(SDIIDMANAGER__INCLUDED_)
