# Railway Network System

## Overview

This project implements a comprehensive railway network system using graph theory and Dijkstra's shortest path algorithm. The system models railway stations as graph nodes and railroad connections as weighted directed edges, enabling efficient computation of optimal travel routes between stations.

Railway Network Visualization is available in the file "railsNetwork 5 12345.svg"

## Technical Architecture

### Core Data Structures

The implementation leverages Haskell's algebraic data types to provide type-safe representations of the railway network components:

```haskell
data PriorityQueue = PQ PQNode PriorityQueue | Empty deriving (Show, Eq)
data PQNode = Node Int Int deriving (Show, Eq)
```

#### PriorityQueue
- **Type**: Recursive algebraic data type
- **Structure**: Min-heap implementation using linked list structure
- **Purpose**: Maintains nodes in ascending order by key (path cost)
- **Invariant**: Head node always contains minimum key value
- **Time Complexity**: O(n) insertion, O(1) extraction

#### PQNode
- **Components**: 
  - Station identifier (Int): 1-indexed station ID
  - Key value (Int): Current shortest known distance from source
- **Range**: Station IDs ∈ [1, n], Keys ∈ [0, 9999]

### Graph Representation

The railway network employs an **adjacency matrix** representation:

- **Type**: `[[Int]]` - 2D list of integers
- **Indexing**: 1-based station identifiers mapped to 0-based array indices
- **Edge Weights**: Range [0, 9] where 0 indicates no connection
- **Properties**:
  - Directed graph (asymmetric matrix)
  - No self-loops (diagonal elements = 0)
  - Dense representation suitable for complete/near-complete graphs

#### Matrix Properties
```
M[i][j] = weight of edge from station (i+1) to station (j+1)
M[i][j] = 0 ⟺ no direct connection exists
M[i][i] = 0 ∀ i (no self-loops)
```

## Algorithm Implementation

### Dijkstra's Shortest Path Algorithm

The system implements Dijkstra's algorithm with the following specifications:

**Input Parameters:**
- Source station identifier
- Destination station identifier  
- Adjacency matrix representation
- Initialized priority queue

**Algorithm Complexity:**
- **Time**: O(V² + E) where V = stations, E = edges
- **Space**: O(V) for priority queue and distance tracking

**Implementation Details:**

1. **Initialization Phase** (`initializeSource`):
   ```haskell
   ∀ v ∈ V: distance[v] = ∞ (9999)
   distance[source] = 0
   Q = priorityQueue(sorted by distance)
   ```

2. **Relaxation Phase** (`computeShortestPathCost`):
   ```haskell
   while Q ≠ ∅:
     u = extractMin(Q)
     if u = destination: return distance[u]
     ∀ v ∈ neighbors(u):
       if distance[u] + weight(u,v) < distance[v]:
         distance[v] = distance[u] + weight(u,v)
         updatePriority(Q, v, distance[v])
   ```

### Random Network Generation

The `railsNetwork` function implements deterministic pseudorandom graph generation:

**Parameters:**
- `numStations`: Network size (number of vertices)
- `seed`: PRNG seed for reproducible generation

**Generation Process:**
1. Initialize linear congruential generator with seed
2. Generate `n²` random values in range [0,9]
3. Construct adjacency matrix with diagonal zeroing
4. Apply matrix transformation to ensure graph properties

**Statistical Properties:**
- Edge probability: ~89% (values 1-9 out of 0-9 range)
- Expected connectivity: High (dense graph)
- Reproducibility: Deterministic given same seed

## API Reference

### Core Functions

#### `railsNetwork :: Int -> Int -> [[Int]]`
Generates a random railway network adjacency matrix.

**Parameters:**
- `numStations`: Number of stations in network (n > 0)
- `seed`: Random seed for reproducible generation

**Returns:** n×n adjacency matrix with integer weights [0,9]

**Example:**
```haskell
railsNetwork 5 12345
-- Returns: [[0,1,0,9,5],[3,0,7,9,8],[9,6,0,0,7],[6,7,1,0,6],[9,5,3,2,0]]
```

#### `pullLever :: Int -> Int -> Int -> [[Int]] -> [[Int]]`
Dynamically modifies edge weights in existing network.

**Parameters:**
- `leverSeed`: Seed for generating new weight
- `station1`: Source station identifier
- `station2`: Destination station identifier  
- `currentMatrix`: Current network state

**Returns:** Modified adjacency matrix with updated edge weight

**Edge Cases:**
- Self-loops (station1 = station2): No modification
- Invalid indices: Returns original matrix unchanged
- Empty matrix: Returns empty matrix

#### `initializeSource :: Int -> [[Int]] -> PriorityQueue`
Initializes priority queue for Dijkstra's algorithm execution.

**Parameters:**
- `src`: Source station identifier
- `matrix`: Network adjacency matrix

**Returns:** Priority queue with source distance = 0, all others = 9999

**Invariant:** Queue maintains ascending order by key values

#### `computeShortestPathCost :: Int -> [[Int]] -> PriorityQueue -> Int`
Computes minimum cost path using Dijkstra's algorithm.

**Parameters:**
- `destination`: Target station identifier
- `matrix`: Network adjacency matrix
- `queue`: Initialized priority queue from `initializeSource`

**Returns:** Minimum path cost from source to destination

**Termination:** Algorithm terminates when destination node is extracted from queue

## Performance Characteristics

### Time Complexity Analysis

| Function | Best Case | Average Case | Worst Case |
|----------|-----------|--------------|------------|
| `railsNetwork` | O(n²) | O(n²) | O(n²) |
| `pullLever` | O(1) | O(n²) | O(n²) |
| `initializeSource` | O(n log n) | O(n log n) | O(n log n) |
| `computeShortestPathCost` | O(n) | O(n²) | O(n³) |

### Space Complexity
- **Matrix Storage**: O(n²) 
- **Priority Queue**: O(n)
- **Total**: O(n²)

## Testing Framework

The implementation includes comprehensive test coverage using HUnit:

### Test Categories

1. **Network Generation Tests** (test1-test5):
   - Validates deterministic generation
   - Checks matrix properties
   - Verifies seed reproducibility

2. **Dynamic Modification Tests** (test6-test10):
   - Edge weight updates
   - Boundary condition handling
   - Matrix integrity preservation

3. **Priority Queue Tests** (test11-test15):
   - Initialization correctness
   - Sorting invariant maintenance
   - Source node prioritization

4. **Shortest Path Tests** (test16-test20):
   - Algorithm correctness
   - Path optimality verification
   - Edge case handling

### Running Tests
```bash
ghci railsNetwork.hs
> runTestTT tests
```

## Dependencies

### Haskell Modules
```haskell
import System.Random    -- Pseudorandom number generation
import Test.HUnit      -- Unit testing framework  
import Data.List       -- List manipulation utilities
import Data.Ord        -- Ordering operations
```

### System Requirements
- GHC (Glasgow Haskell Compiler) ≥ 8.0
- Haskell Platform or Stack
- Memory: O(n²) where n = maximum network size

## Implementation Notes

### Design Decisions

1. **1-based Indexing**: Station identifiers use 1-based indexing for intuitive user interface
2. **Dense Matrix**: Adjacency matrix chosen over adjacency list for O(1) edge lookup
3. **Immutable Updates**: Functional approach ensures data structure integrity
4. **Lazy Evaluation**: Haskell's lazy evaluation optimizes memory usage for large networks

### Algorithmic Optimizations

1. **Priority Queue**: Custom implementation optimized for frequent minimum extractions
2. **Matrix Access**: Direct array indexing for O(1) edge weight retrieval  
3. **Early Termination**: Dijkstra's algorithm terminates upon reaching destination
4. **Sorting Stability**: Maintains consistent ordering for deterministic behavior

### Error Handling

- **Bounds Checking**: Validates station identifiers against matrix dimensions
- **Empty Network**: Graceful handling of zero-station networks
- **Invalid Seeds**: Accepts any integer seed value
- **Disconnected Graphs**: Returns infinity (9999) for unreachable destinations

## Usage Examples

### Basic Network Creation and Pathfinding

```haskell
-- Generate 5-station network
let network = railsNetwork 5 12345

-- Initialize for shortest path from station 2
let pq = initializeSource 2 network

-- Find shortest path cost from station 2 to station 4
let cost = computeShortestPathCost 4 network pq

-- Modify edge weight between stations 3 and 4
let modifiedNetwork = pullLever 123 3 4 network
```

### Batch Processing Example

```haskell
-- Generate multiple network configurations
let networks = map (railsNetwork 10) [1..100]

-- Compute all-pairs shortest paths for each network
let pathCosts = map computeAllPaths networks
```

