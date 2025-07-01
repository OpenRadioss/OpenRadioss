
#include <vector>
#include <queue>
#include <unordered_set>
#include <unordered_map>
#include <algorithm>
#include <cmath>
#include <limits>
#include <iostream>
#include <cassert>
#include <random>

// Custom hash function for std::pair<int,int>
struct PairHash
{
    size_t operator()(const std::pair<int, int> &p) const
    {
        return std::hash<int>()(p.first) ^ (std::hash<int>()(p.second) << 1);
    }
};

// C++11 compatible sampling function
template <typename T>
std::vector<T> sample_vector(const std::vector<T> &input, size_t sample_size, std::mt19937 &gen)
{
    if (input.size() <= sample_size)
    {
        return input;
    }

    std::vector<T> result = input;
    std::shuffle(result.begin(), result.end(), gen);
    result.resize(sample_size);
    return result;
}

class GraphConnectivityAugmentor
{
private:
    int num_vertices;
    std::vector<int> xadj;
    std::vector<int> adjncy;
    std::vector<float> coordinates;
    double max_edge_length_ratio;

    // ==================== UTILITY METHODS ====================

    // Helper function to compute 3D distance between two vertices
    double compute_distance(int vertex1, int vertex2) const
    {
        float dx = coordinates[3 * vertex1 + 0] - coordinates[3 * vertex2 + 0];
        float dy = coordinates[3 * vertex1 + 1] - coordinates[3 * vertex2 + 1];
        float dz = coordinates[3 * vertex1 + 2] - coordinates[3 * vertex2 + 2];
        return std::sqrt(dx * dx + dy * dy + dz * dz);
    }

    // Helper function to get neighbors of a vertex
    std::vector<int> get_neighbors(int vertex) const
    {
        std::vector<int> neighbors;
        const int degree = xadj[vertex + 1] - xadj[vertex];
        neighbors.reserve(degree);
        for (int i = xadj[vertex]; i < xadj[vertex + 1]; ++i)
        {
            neighbors.push_back(adjncy[i]);
        }
        return neighbors;
    }

    // Get k-ring neighbors of a vertex (vertices within k hops)
    std::unordered_set<int> get_k_ring_neighbors(int vertex, int k) const
    {
        std::unordered_set<int> neighbors;
        std::unordered_set<int> current_ring;
        current_ring.insert(vertex);

        for (int ring = 0; ring < k && !current_ring.empty(); ++ring)
        {
            std::unordered_set<int> next_ring;

            for (std::unordered_set<int>::const_iterator it = current_ring.begin();
                 it != current_ring.end(); ++it)
            {
                int v = *it;
                for (int i = xadj[v]; i < xadj[v + 1]; ++i)
                {
                    int neighbor = adjncy[i];
                    if (neighbors.find(neighbor) == neighbors.end())
                    {
                        next_ring.insert(neighbor);
                        neighbors.insert(neighbor);
                    }
                }
            }
            current_ring = next_ring;
        }

        neighbors.erase(vertex);
        return neighbors;
    }

    // ==================== GRAPH MODIFICATION METHODS ====================

    // Add edge to the graph structure
    void add_edge_to_graph(int u, int v)
    {
        // Find insertion position for u's adjacency list
        auto pos_u = std::lower_bound(adjncy.begin() + xadj[u],
                                      adjncy.begin() + xadj[u + 1], v);

        // Insert v in u's adjacency list if not already present
        if (pos_u == adjncy.begin() + xadj[u + 1] || *pos_u != v)
        {
            int insert_idx_u = pos_u - adjncy.begin();
            adjncy.insert(adjncy.begin() + insert_idx_u, v);

            // Update xadj indices for vertices after u
            for (int i = u + 1; i <= num_vertices; ++i)
            {
                xadj[i]++;
            }
        }

        // Find insertion position for v's adjacency list
        auto pos_v = std::lower_bound(adjncy.begin() + xadj[v],
                                      adjncy.begin() + xadj[v + 1], u);

        // Insert u in v's adjacency list if not already present
        if (pos_v == adjncy.begin() + xadj[v + 1] || *pos_v != u)
        {
            int insert_idx_v = pos_v - adjncy.begin();
            adjncy.insert(adjncy.begin() + insert_idx_v, u);

            // Update xadj indices for vertices after v
            for (int i = v + 1; i <= num_vertices; ++i)
            {
                xadj[i]++;
            }
        }
    }

    // ==================== CONNECTIVITY ANALYSIS METHODS ====================

    // BFS to find connected components
    std::vector<std::vector<int>> find_connected_components() const
    {
        std::vector<bool> visited(num_vertices, false);
        std::vector<std::vector<int>> components;
        components.reserve(10); // Reserve space for up to 10 components

        for (int i = 0; i < num_vertices; ++i)
        {
            if (!visited[i])
            {
                std::vector<int> component;
                std::queue<int> q;
                q.push(i);
                visited[i] = true;

                while (!q.empty())
                {
                    int v = q.front();
                    q.pop();
                    component.push_back(v);

                    for (int neighbor : get_neighbors(v))
                    {
                        if (!visited[neighbor])
                        {
                            visited[neighbor] = true;
                            q.push(neighbor);
                        }
                    }
                }
                components.push_back(component);
            }
        }
        return components;
    }

    // Find bridges using Tarjan's algorithm (early termination optimization)
    bool find_first_bridge(std::pair<int, int> &bridge) const
    {
        std::vector<int> disc(num_vertices, -1);
        std::vector<int> low(num_vertices, -1);
        std::vector<bool> visited(num_vertices, false);
        int time_counter = 0;

        for (int i = 0; i < num_vertices; ++i)
        {
            if (!visited[i])
            {
                if (find_bridges_dfs_early(i, -1, time_counter, disc, low, visited, bridge))
                {
                    return true; // Found first bridge, early termination
                }
            }
        }
        return false;
    }

    bool find_bridges_dfs_early(int u, int parent, int &time_counter,
                                std::vector<int> &disc, std::vector<int> &low,
                                std::vector<bool> &visited,
                                std::pair<int, int> &bridge) const
    {
        visited[u] = true;
        disc[u] = low[u] = ++time_counter;

        for (int v : get_neighbors(u))
        {
            if (v == parent)
                continue; // Skip back edge to parent

            if (!visited[v])
            {
                if (find_bridges_dfs_early(v, u, time_counter, disc, low, visited, bridge))
                {
                    return true; // Early termination
                }
                low[u] = std::min(low[u], low[v]);

                // If low[v] > disc[u], then (u,v) is a bridge
                if (low[v] > disc[u])
                {
                    bridge = std::make_pair(std::min(u, v), std::max(u, v));
                    return true; // Found bridge, early termination
                }
            }
            else
            {
                low[u] = std::min(low[u], disc[v]);
            }
        }
        return false;
    }

    // ==================== MAX FLOW FOR EDGE CONNECTIVITY ====================

    struct FlowEdge
    {
        int to, rev;
        int cap;
        FlowEdge(int to, int rev, int cap) : to(to), rev(rev), cap(cap) {}
    };

    int max_flow_edge_connectivity(int source, int sink, std::vector<std::pair<int, int>> &min_cut_edges)
    {
        // Create flow network - each original edge becomes two directed edges with capacity 1
        std::vector<std::vector<FlowEdge>> flow_graph(num_vertices);
        const int total_edges = adjncy.size();
        const int avg_degree = total_edges / num_vertices;
        for (int u = 0; u < num_vertices; ++u)
        {
            flow_graph[u].reserve(avg_degree); // Reserve space for average degree
        }
        // Build flow network from original graph
        for (int u = 0; u < num_vertices; ++u)
        {
            for (int neighbor : get_neighbors(u))
            {
                if (u < neighbor)
                { // Process each undirected edge once
                    // Add directed edge u -> neighbor
                    int u_idx = flow_graph[u].size();
                    int v_idx = flow_graph[neighbor].size();
                    flow_graph[u].emplace_back(neighbor, v_idx, 1);
                    flow_graph[neighbor].emplace_back(u, u_idx, 1);
                }
            }
        }

        int max_flow = 0;
        while (true)
        {
            std::vector<int> parent(num_vertices, -1);
            std::vector<int> parent_edge(num_vertices, -1);
            std::queue<int> q;
            q.push(source);
            parent[source] = source;

            // BFS to find augmenting path
            while (!q.empty() && parent[sink] == -1)
            {
                int v = q.front();
                q.pop();

                for (int i = 0; i < flow_graph[v].size(); ++i)
                {
                    const FlowEdge &edge = flow_graph[v][i];
                    if (parent[edge.to] == -1 && edge.cap > 0)
                    {
                        parent[edge.to] = v;
                        parent_edge[edge.to] = i;
                        q.push(edge.to);
                    }
                }
            }

            if (parent[sink] == -1)
                break; // No more augmenting paths

            // Find minimum capacity along the path (should be 1 for edge connectivity)
            int path_flow = 1;

            // Update residual capacities
            for (int v = sink; v != source; v = parent[v])
            {
                int u = parent[v];
                int edge_idx = parent_edge[v];
                flow_graph[u][edge_idx].cap -= path_flow;
                flow_graph[v][flow_graph[u][edge_idx].rev].cap += path_flow;
            }

            max_flow += path_flow;
        }

        // Find min-cut edges
        std::vector<bool> reachable(num_vertices, false);
        std::queue<int> q;
        q.push(source);
        reachable[source] = true;

        while (!q.empty())
        {
            int v = q.front();
            q.pop();

            for (const FlowEdge &edge : flow_graph[v])
            {
                if (!reachable[edge.to] && edge.cap > 0)
                {
                    reachable[edge.to] = true;
                    q.push(edge.to);
                }
            }
        }

        // Collect cut edges (edges from reachable to non-reachable vertices)
        min_cut_edges.clear();
        for (int u = 0; u < num_vertices; ++u)
        {
            if (reachable[u])
            {
                for (int v : get_neighbors(u))
                {
                    if (!reachable[v])
                    {
                        min_cut_edges.emplace_back(std::min(u, v), std::max(u, v));
                    }
                }
            }
        }

        // Remove duplicates
        std::sort(min_cut_edges.begin(), min_cut_edges.end());
        min_cut_edges.erase(std::unique(min_cut_edges.begin(), min_cut_edges.end()), min_cut_edges.end());

        return max_flow;
    }

    // Compute edge connectivity of the graph (called only once now)
    int compute_edge_connectivity()
    {
        auto components = find_connected_components();
        if (components.size() > 1)
        {
            return 0; // Disconnected graph
        }

        // Quick check: if there are any bridges, edge connectivity is 1
        std::pair<int, int> first_bridge;
        if (find_first_bridge(first_bridge))
        {
            return 1;
        }

        int min_connectivity = std::numeric_limits<int>::max();

        // Try different source-sink pairs to find minimum cut
        for (int trials = 0; trials < std::min(10, num_vertices - 1); ++trials)
        {
            int source = trials;
            int sink = (trials + num_vertices / 2) % num_vertices;
            if (source == sink)
                sink = (sink + 1) % num_vertices;

            std::vector<std::pair<int, int>> cut_edges;
            int connectivity = max_flow_edge_connectivity(source, sink, cut_edges);
            min_connectivity = std::min(min_connectivity, connectivity);
        }

        return min_connectivity;
    }

    // ==================== MINIMUM CUT METHODS ====================

    // Find minimum cut that separates the graph
    std::pair<std::vector<int>, std::vector<int>> find_minimum_cut()
    {
        // Quick check: if there are bridges, use the first bridge to create cut
        std::pair<int, int> first_bridge;
        if (find_first_bridge(first_bridge))
        {
            return find_cut_by_removing_edge(first_bridge.first, first_bridge.second);
        }

        int min_cut_value = std::numeric_limits<int>::max();
        std::vector<std::pair<int, int>> best_cut_edges;

        // Try multiple source-sink pairs
        for (int trials = 0; trials < std::min(20, num_vertices - 1); ++trials)
        {
            int source = trials;
            int sink = (trials + num_vertices / 2) % num_vertices;
            if (source == sink)
                sink = (sink + 1) % num_vertices;

            std::vector<std::pair<int, int>> cut_edges;
            int cut_value = max_flow_edge_connectivity(source, sink, cut_edges);

            if (cut_value < min_cut_value)
            {
                min_cut_value = cut_value;
                best_cut_edges = cut_edges;
            }
        }

        // Convert cut edges to vertex sets
        return find_cut_by_removing_edges(best_cut_edges);
    }

    // Helper function to find connected components after removing an edge
    std::pair<std::vector<int>, std::vector<int>> find_cut_by_removing_edge(int u, int v)
    {
        std::vector<bool> visited(num_vertices, false);
        std::vector<int> component1;

        // BFS from u, avoiding the edge (u,v)
        std::queue<int> q;
        q.push(u);
        visited[u] = true;

        while (!q.empty())
        {
            int curr = q.front();
            q.pop();
            component1.push_back(curr);

            for (int neighbor : get_neighbors(curr))
            {
                if (!visited[neighbor] && !((curr == u && neighbor == v) || (curr == v && neighbor == u)))
                {
                    visited[neighbor] = true;
                    q.push(neighbor);
                }
            }
        }

        // Collect remaining vertices
        std::vector<int> component2;
        for (int i = 0; i < num_vertices; ++i)
        {
            if (!visited[i])
            {
                component2.push_back(i);
            }
        }

        return std::make_pair(component1, component2);
    }

    // Helper function to find connected components after removing multiple edges
    std::pair<std::vector<int>, std::vector<int>> find_cut_by_removing_edges(
        const std::vector<std::pair<int, int>> &cut_edges)
    {

        std::unordered_set<std::pair<int, int>, PairHash> removed_edges;
        for (const auto &edge : cut_edges)
        {
            removed_edges.insert(edge);
        }

        std::vector<bool> visited(num_vertices, false);
        std::vector<int> component1;

        // BFS from vertex 0, avoiding cut edges
        std::queue<int> q;
        q.push(0);
        visited[0] = true;

        while (!q.empty())
        {
            int curr = q.front();
            q.pop();
            component1.push_back(curr);

            for (int neighbor : get_neighbors(curr))
            {
                if (!visited[neighbor])
                {
                    std::pair<int, int> edge = std::make_pair(std::min(curr, neighbor), std::max(curr, neighbor));
                    if (removed_edges.find(edge) == removed_edges.end())
                    {
                        visited[neighbor] = true;
                        q.push(neighbor);
                    }
                }
            }
        }

        // Collect remaining vertices
        std::vector<int> component2;
        for (int i = 0; i < num_vertices; ++i)
        {
            if (!visited[i])
            {
                component2.push_back(i);
            }
        }

        return std::make_pair(component1, component2);
    }

    // ==================== SPATIAL EDGE FINDING METHODS ====================

    std::vector<std::pair<int, int>> find_initial_candidates(
        const std::vector<int> &set_A, const std::vector<int> &set_B, int num_edges_needed)
    {

        size_t sample_size = std::min(static_cast<size_t>(200), std::min(set_A.size(), set_B.size()));

        std::mt19937 gen(42);

        std::vector<int> sample_A = sample_vector(set_A, sample_size, gen);
        std::vector<int> sample_B = sample_vector(set_B, sample_size, gen);

        std::vector<std::tuple<double, int, int>> distances;
        distances.reserve(sample_A.size() * sample_B.size());

        // Compute ALL distances - no spatial constraint
        for (size_t i = 0; i < sample_A.size(); ++i)
        {
            for (size_t j = 0; j < sample_B.size(); ++j)
            {
                int u = sample_A[i];
                int v = sample_B[j];
                double dist = compute_distance(u, v);
                distances.push_back(std::make_tuple(dist, u, v));
            }
        }

        std::sort(distances.begin(), distances.end());

        std::vector<std::pair<int, int>> candidates;
        std::unordered_set<std::pair<int, int>, PairHash> added;

        size_t max_candidates = std::min(distances.size(), static_cast<size_t>(num_edges_needed * 3));
        candidates.reserve(max_candidates);

        for (size_t i = 0; i < max_candidates; ++i)
        {
            int u = std::get<1>(distances[i]);
            int v = std::get<2>(distances[i]);
            std::pair<int, int> edge = std::make_pair(std::min(u, v), std::max(u, v));

            if (added.find(edge) == added.end())
            {
                candidates.push_back(std::make_pair(u, v));
                added.insert(edge);
            }
        }

        return candidates;
    }

    std::vector<std::pair<int, int>> refine_candidates(
        const std::vector<int> &set_A, const std::vector<int> &set_B,
        const std::vector<std::pair<int, int>> &initial_candidates, int num_edges_needed)
    {

        std::vector<std::tuple<double, int, int>> refined_distances;
        std::unordered_set<std::pair<int, int>, PairHash> checked;

        std::unordered_set<int> set_A_lookup(set_A.begin(), set_A.end());
        std::unordered_set<int> set_B_lookup(set_B.begin(), set_B.end());

        for (size_t i = 0; i < initial_candidates.size(); ++i)
        {
            int u_center = initial_candidates[i].first;
            int v_center = initial_candidates[i].second;

            std::unordered_set<int> u_neighbors = get_k_ring_neighbors(u_center, 2);
            std::unordered_set<int> v_neighbors = get_k_ring_neighbors(v_center, 2);

            std::vector<int> valid_u, valid_v;
            valid_u.reserve(u_neighbors.size() + 1);
            valid_v.reserve(v_neighbors.size() + 1);

            for (std::unordered_set<int>::const_iterator it = u_neighbors.begin();
                 it != u_neighbors.end(); ++it)
            {
                if (set_A_lookup.count(*it))
                    valid_u.push_back(*it);
            }

            for (std::unordered_set<int>::const_iterator it = v_neighbors.begin();
                 it != v_neighbors.end(); ++it)
            {
                if (set_B_lookup.count(*it))
                    valid_v.push_back(*it);
            }

            valid_u.push_back(u_center);
            valid_v.push_back(v_center);

            for (size_t ui = 0; ui < valid_u.size(); ++ui)
            {
                for (size_t vi = 0; vi < valid_v.size(); ++vi)
                {
                    int u = valid_u[ui];
                    int v = valid_v[vi];
                    std::pair<int, int> edge = std::make_pair(std::min(u, v), std::max(u, v));

                    if (checked.find(edge) == checked.end())
                    {
                        checked.insert(edge);
                        double dist = compute_distance(u, v);
                        refined_distances.push_back(std::make_tuple(dist, u, v));
                    }
                }
            }
        }

        std::sort(refined_distances.begin(), refined_distances.end());

        std::vector<std::pair<int, int>> final_edges;
        std::unordered_set<std::pair<int, int>, PairHash> added;

        for (size_t i = 0; i < refined_distances.size() &&
                           final_edges.size() < static_cast<size_t>(num_edges_needed);
             ++i)
        {

            int u = std::get<1>(refined_distances[i]);
            int v = std::get<2>(refined_distances[i]);
            std::pair<int, int> edge = std::make_pair(std::min(u, v), std::max(u, v));

            if (added.find(edge) == added.end())
            {
                final_edges.push_back(std::make_pair(u, v));
                added.insert(edge);
            }
        }

        return final_edges;
    }

    std::vector<std::pair<int, int>> find_with_expanded_search(
        const std::vector<int> &set_A, const std::vector<int> &set_B, int num_edges_needed)
    {

        size_t sample_size = std::min(static_cast<size_t>(1000), std::min(set_A.size(), set_B.size()));

        std::mt19937 gen(42);

        std::vector<int> sample_A = sample_vector(set_A, sample_size, gen);
        std::vector<int> sample_B = sample_vector(set_B, sample_size, gen);

        std::vector<std::tuple<double, int, int>> distances;

        // Compute ALL distances - no spatial constraint
        for (size_t i = 0; i < sample_A.size(); ++i)
        {
            for (size_t j = 0; j < sample_B.size(); ++j)
            {
                int u = sample_A[i];
                int v = sample_B[j];
                double dist = compute_distance(u, v);
                distances.push_back(std::make_tuple(dist, u, v));
            }
        }

        size_t k = std::min(distances.size(), static_cast<size_t>(num_edges_needed * 2));
        std::partial_sort(distances.begin(), distances.begin() + k, distances.end());

        std::vector<std::pair<int, int>> edges;
        edges.reserve(num_edges_needed * 2);
        std::unordered_set<std::pair<int, int>, PairHash> added;

        for (size_t i = 0; i < k && edges.size() < static_cast<size_t>(num_edges_needed); ++i)
        {
            int u = std::get<1>(distances[i]);
            int v = std::get<2>(distances[i]);
            std::pair<int, int> edge = std::make_pair(std::min(u, v), std::max(u, v));

            if (added.find(edge) == added.end())
            {
                edges.push_back(std::make_pair(u, v));
                added.insert(edge);
            }
        }

        return edges;
    }

    std::vector<std::pair<int, int>> find_shortest_spatial_edges(
        const std::vector<int> &set_A,
        const std::vector<int> &set_B,
        int num_edges_needed)
    {

        // Stage 1: Coarse sampling (200x200)
        std::vector<std::pair<int, int>> candidates = find_initial_candidates(set_A, set_B, num_edges_needed);

        if (candidates.size() >= static_cast<size_t>(num_edges_needed))
        {
            return refine_candidates(set_A, set_B, candidates, num_edges_needed);
        }

        return find_with_expanded_search(set_A, set_B, num_edges_needed);
    }

    // ==================== COMPONENT CONNECTION METHODS ====================

    // Connect disconnected components using shortest spatial distances
    std::vector<std::pair<int, int>> connect_components_spatially(
        const std::vector<std::vector<int>> &components)
    {

        std::vector<std::pair<int, int>> edges;
        edges.reserve(components.size() - 1);
        std::vector<std::vector<int>> remaining_components = components;
        std::mt19937 gen(42);

        // Connect components in minimum spanning tree fashion

        // Find the largest component to sample
        size_t largest_component_size = 0;
        size_t largest_component_index = 0;
        for (size_t i = 0; i < remaining_components.size(); ++i)
        {
            if (remaining_components[i].size() > largest_component_size)
            {
                largest_component_size = remaining_components[i].size();
                largest_component_index = i;
            }
        }

        size_t i = largest_component_index;
        std::vector<int> sample_i = sample_vector(remaining_components[i], 1000, gen);
        for (size_t j = 0; j < remaining_components.size(); ++j)
        {
            if (j == i)
                continue; // Skip the largest component
            std::vector<int> sample_j = sample_vector(remaining_components[j], 100, gen);
            double min_distance = std::numeric_limits<double>::max();
            int best_comp1 = -1, best_comp2 = -1;
            int best_u = -1, best_v = -1;
            int second_best_comp1 = -1, second_best_comp2 = -1;
            int second_best_u = -1, second_best_v = -1;

            for (int u : sample_i)
            {
                for (int v : sample_j)
                {
                    double dist = compute_distance(u, v);
                    if (dist < min_distance)
                    {
                        min_distance = dist;
                        if (best_comp1 != -1)
                        {
                            second_best_comp1 = best_comp1;
                            second_best_comp2 = best_comp2;
                            second_best_u = best_u;
                            second_best_v = best_v;
                        }
                        best_comp1 = i;
                        best_comp2 = j;
                        best_u = u;
                        best_v = v;
                    }
                }
            }

            if (best_comp1 != -1)
            {
                // Add the connecting edge
                edges.emplace_back(best_u, best_v);
                if (second_best_comp1 != -1)
                {
                    edges.emplace_back(second_best_u, second_best_v);
                }
            }
            else
            {
                break; // Should not happen
            }
        }
        return edges;
    }

public:
    GraphConnectivityAugmentor(const std::vector<int> &xadj_input,
                               const std::vector<int> &adjncy_input,
                               const std::vector<float> &coords,
                               double max_edge_ratio = 2.0)
        : num_vertices(xadj_input.size() - 1),
          xadj(xadj_input),
          adjncy(adjncy_input),
          coordinates(coords),
          max_edge_length_ratio(max_edge_ratio)
    {

        assert(coordinates.size() == 3 * num_vertices);
        assert(xadj.size() == num_vertices + 1);
    }

    // ==================== MAIN PUBLIC INTERFACE ====================

    // Main function: augment graph to achieve k-edge connectivity
    std::vector<std::pair<int, int>> augment_to_k_edge_connectivity(int k)
    {
        std::vector<std::pair<int, int>> augmentation_edges;

        // Handle disconnected components first
        auto components = find_connected_components();
        if (components.size() > 1)
        {
            auto component_edges = connect_components_spatially(components);
            augmentation_edges.insert(augmentation_edges.end(),
                                      component_edges.begin(), component_edges.end());
        }
        components = find_connected_components(); // Recompute after connecting components
        if (components.size() > 1)
        {
            std::cout << "Warning: Graph still has " << components.size() << " disconnected components after augmentation ";
            std::cout <<" even if we added "<< augmentation_edges.size() << " edges." << std::endl;
        }

        // Compute connectivity only once!
        int current_connectivity = compute_edge_connectivity();

        if (current_connectivity >= k)
        {
            return augmentation_edges;
        }

        // Find minimum cut
        std::pair<std::vector<int>, std::vector<int>> cut_result = find_minimum_cut();
        std::vector<int> set_A = cut_result.first;
        std::vector<int> set_B = cut_result.second;

        if (set_A.empty() || set_B.empty())
        {
            return augmentation_edges;
        }

        // Determine how many edges to add
        int edges_needed = k - current_connectivity;

        // Find shortest spatial edges across the cut
        auto new_edges = find_shortest_spatial_edges(set_A, set_B, edges_needed);

        if (new_edges.empty())
        {
            // Relax spatial constraints and try again
            max_edge_length_ratio *= 2.0;
            new_edges = find_shortest_spatial_edges(set_A, set_B, edges_needed);

            if (new_edges.empty())
            {
                return augmentation_edges;
            }
        }

        // Add edges to graph and result
        for (const auto &edge : new_edges)
        {
            add_edge_to_graph(edge.first, edge.second);
            augmentation_edges.push_back(edge);
        }

        return augmentation_edges;
    }
};

// Convenience function for C-style interface
std::vector<std::pair<int, int>> augment_graph_connectivity(
    const std::vector<int> &xadj,
    const std::vector<int> &adjncy,
    const std::vector<float> &coordinates,
    int k,
    double max_edge_length_ratio = 2.0)
{

    GraphConnectivityAugmentor augmentor(xadj, adjncy, coordinates, max_edge_length_ratio);
    return augmentor.augment_to_k_edge_connectivity(k);
}