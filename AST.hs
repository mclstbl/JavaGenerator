module AST (
	Arrangement(..),
	ClassName(..),
	Constructors(..),
	DeterminantMethod(..),
	Errors(..),
	Format(..),
	getClassName,
	getDimension,
	JavaMatrix(..),
	makeJavaData,
	Matrix(..),
	Variables
	) where

{-------------------------------------------------------------
	ClassName 	-- 	Record type Matrix inputs have dxd, where
					d is the dimension, in their ClassName
-------------------------------------------------------------}
type ClassName = String
{-------------------------------------------------------------
	Determinant Function
-------------------------------------------------------------}
data DeterminantMethod = Determinant3x3
	| DeterminantGeneral
	deriving(Show)
{-------------------------------------------------------------
	Errors
-------------------------------------------------------------}
data Errors = NotSquareMatrixException 
	| InvalidArrayLengthException
	deriving(Show)
{-------------------------------------------------------------
	JavaMatrix containts data about what should be in the 
	generated Java file
-------------------------------------------------------------}
data JavaMatrix = 
	MS 
	ClassName 
	Variables 
	Errors 
	Constructors 
	DeterminantMethod
	deriving(Show)
{-------------------------------------------------------------
	Matrix is parametized by Record/Array, RC/CR/Flat, Dimension
-------------------------------------------------------------}
data Matrix = M Format Arrangement Dimension

type Dimension = Int

data Format = Array | Record

data Arrangement = CR | Flat | RC
{-------------------------------------------------------------
	Variables
-------------------------------------------------------------}
type Variables = [String]
{-------------------------------------------------------------
	Constructors -- 3x3 code has 1 constructor
				 -- nxn code has 2 constructors
				 -- for the sake of testing, I have omitted
				 	them in the final code to show that the
				 	determinant works
-------------------------------------------------------------}
data Constructors = OverloadConstructor Constructors
	| Constructor
	deriving(Show)
{-------------------------------------------------------------
	Transitions	-- 	These functions create constructs for the
					Java code, depending on combinations of
					Record/Array, Flat/RC/CR and Dimension
-------------------------------------------------------------}
makeJavaData :: Matrix -> JavaMatrix
makeJavaData (M Record Flat d) = 
	MS 
	("Matrix" ++ (show d) ++ "x" ++ (show d) ++ "Flat" )
	["Matrix.a","","","","Flat","a","","",""] 
	(NotSquareMatrixException) -- InvalidArrayLengthException) 
	(OverloadConstructor Constructor) 
	DeterminantGeneral
makeJavaData (M Record RC d) = 
	MS 
	("Matrix" ++ (show d) ++ "x" ++ (show d) ++ "RC")
	["Matrix.row","column",".","]","MatrixRC","Row","row","","column"] 
	(NotSquareMatrixException) -- InvalidArrayLengthException) 
	(OverloadConstructor Constructor) 
	DeterminantGeneral
makeJavaData (M Record CR d) = 
	MS 
	("Matrix" ++ (show d) ++ "x" ++ (show d) ++ "CR")
	["Matrix.column","row",".","]","MatrixCR","Column","column","","row"] 
	(NotSquareMatrixException) -- InvalidArrayLengthException) 
	(OverloadConstructor Constructor) 
	DeterminantGeneral
--------------------------------------------------------------
--------------------------------------------------------------
makeJavaData (M Array Flat d) = 
	MS 
	"MatrixArrayFlat" 
	["Matrix","","1","0","i","j","k","","Flat"] 
	(NotSquareMatrixException) -- InvalidArrayLengthException) 
	(OverloadConstructor Constructor) 
	DeterminantGeneral
makeJavaData (M Array RC d) = 
	MS 
	"MatrixArrayRC" 
	["columns","rows","1","0","i","j","k","j","[0]"] 
	(NotSquareMatrixException) -- InvalidArrayLengthException) 
	(OverloadConstructor Constructor) 
	DeterminantGeneral
makeJavaData (M Array CR d) = 
	MS 
	"MatrixArrayCR" 
	["rows","columns","0","1","j","i","j","k","[i]"] 
	(NotSquareMatrixException) -- InvalidArrayLengthException) 
	(OverloadConstructor Constructor) 
	DeterminantGeneral

{-------------------------------------------------------------
	To get ClassName and Dimension from JavaMatrix
-------------------------------------------------------------}
getClassName :: JavaMatrix -> String
getClassName (MS className _ _ _ _) = className

getDimension :: Matrix -> Dimension
getDimension (M _ _ d) = d





