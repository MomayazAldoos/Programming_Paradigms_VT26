# Programming Paradigms VT2026

A comprehensive collection of functional programming implementations in Haskell, demonstrating advanced programming concepts and mathematical computing.

## 📂 Lab Structure

### Lab 4: Functional Programming Fundamentals
`funktionell_programmering.hs` - Complete Haskell implementation covering basic functional programming concepts:
- Recursive function design
- Pattern matching and guards  
- List processing techniques
- Higher-order functions
- I/O operations in functional programming

### Lab 5: Rose Trees & Matrix Operations

**Files:**
- `rosTräd.hs` - Advanced data structures and operations
- Complete implementation of Rose Trees and Matrix operations

**Implementation highlights:**

## 🌳 Rose Tree Implementation

### Data Structure
```haskell
data RoseTree a = N a [RoseTree a]
```

### Core Functions Implemented

#### 1. Tree Traversal Operations
- **`leaves`**: Extract all leaf nodes from the tree
- **`flatten`**: Convert tree to flat list (pre-order traversal)

#### 2. Higher-Order Function
- **`mapRoseTree`**: Apply any function to all nodes in the tree
  ```haskell
  mapRoseTree :: (a -> b) -> RoseTree a -> RoseTree b
  ```

## 🔢 Matrix Operations Suite

### Innovative Sparse Matrix Representation
```haskell
data Matrix a = Empty | Cons a [a] [a] (Matrix a)
```
Where: `Cons corner_element row_rest column_rest submatrix`

### Complete Function Library

#### 1. Conversion Functions
- **`matrixToList`**: Convert matrix to 2D list representation
- **`listToMatrix`**: Build matrix from 2D list (with robust edge case handling)

#### 2. Higher-Order Operations  
- **`mapMatrix`**: Apply function to every matrix element
- **`negMatrix`**: Negate all matrix elements (using mapMatrix)
- **`scalarMul`**: Scalar multiplication (leveraging functional composition)

#### 3. Linear Algebra Operations
- **`addMatrix`**: Matrix addition with dimension compatibility
- **`subMatrix`**: Matrix subtraction (elegant composition: `addMatrix m (negMatrix n)`)
- **`mulColRow`**: Outer product of column and row vectors

## 💡 Key Technical Achievements

### 1. **Advanced Pattern Matching**
- Complex recursive structures with multiple base cases
- Safe handling of edge cases (empty matrices, mismatched dimensions)

### 2. **Functional Composition**
- `subMatrix` built on top of `addMatrix` and `negMatrix`
- `negMatrix` and `scalarMul` as specializations of `mapMatrix`

### 3. **Higher-Order Programming**
- Extensive use of `map`, `zipWith`, and custom higher-order functions
- Function composition and currying throughout

### 4. **Robust Error Handling**
- Comprehensive pattern matching for all edge cases
- Type-safe operations with Haskell's type system

## 🧪 Comprehensive Testing

Each function includes extensive test suites with:
- **Edge cases**: Empty structures, single elements
- **Complex cases**: Large matrices, deeply nested trees  
- **Mathematical verification**: All operations mathematically validated

### Example Test Results
```haskell
> addMatrix m1 m1
Cons 2 [4] [6] (Cons 8 [] [] Empty)

> mulColRow [1,2] [3,4]  
Cons 3 [4] [6] (Cons 8 [] [] Empty)

> mapRoseTree (*10) tree
-- Multiplies every tree node by 10
```

## 🔧 Technologies & Concepts Demonstrated

- **Haskell**: Advanced functional programming
- **Algebraic Data Types**: Custom data structure design
- **Recursion**: Complex recursive algorithms
- **Higher-Order Functions**: Function composition and abstraction
- **Linear Algebra**: Matrix operations and vector mathematics
- **Type Safety**: Leveraging Haskell's type system for robust code

## 📊 Code Quality Features

- **Pure Functions**: All operations are side-effect free
- **Type Safety**: Compile-time error prevention
- **Modularity**: Functions built on reusable components
- **Mathematical Correctness**: All operations follow mathematical principles
- **Comprehensive Documentation**: Clear function signatures and examples

## 🎯 Skills Demonstrated

This project showcases proficiency in:
- Functional programming paradigms
- Advanced data structure design
- Mathematical computing
- Algorithm optimization
- Type-driven development
- Test-driven development

---

*This project demonstrates advanced functional programming skills applicable to domains requiring mathematical computing, data analysis, and algorithm development.*