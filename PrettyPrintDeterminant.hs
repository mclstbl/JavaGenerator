module PrettyPrintDeterminant where
{-	
	JavaMatrix types are processed and produce Strings which are part of the final Java code
	all functions here produce Strings because they directly go into the template
-}
import AST (
	Arrangement(..),
	ClassName(..),
	Constructors(..),
	DeterminantMethod(..),
	Errors(..),
	Format(..),
	getDimension,
	JavaMatrix(..),
	makeJavaData,
	Matrix(..),
	Variables
	) 
import Data.List

-- RECORD methods must follow their corresponding array methods
-- Flat -> Flat , RC -> RC, CR -> CR
-- CR and RC look the same for Arrays 
-- Instead of loops, we use variables for Record types

makeDeterminantMethod :: Matrix -> JavaMatrix -> [String]
{-
	This is the template for Array matrix determinant methods
-}
makeDeterminantMethod (M Array Flat d) (MS className [r,c,b1,b2,i,j,k,j2,v2] e constructor det) =
	[
	"\tpublic long determinant() throws NotSquareMatrixException {",
	"\t\tlong det = 0, s;"
	] ++
	(arrayCopyHelper (M Array Flat d) (MS className [r,c,b1,b2,i,j,k,j2,v2] e constructor det)) ++
	[
	"\t\tif(n == 1) {",
    "\t\t\treturn(Matrix" ++ v2 ++ "[0]);",
    "\t\t} else {",
    "\t\t\tfor(int i = 0; i < n; i++) {",
    "\t\t\t\tlong[] smallerMatrix = new long[(n - 1) * (n - 1)];",
    "\t\t\t\tfor(int " ++ j ++ " = " ++ b1 ++ " ; " ++ j ++ " < n; " ++ j ++ "++) {",
    "\t\t\t\t\tfor(int " ++ k ++ " = " ++ b2 ++ "; " ++ k ++ " < n; " ++ k ++ "++) {",
    "\t\t\t\t\t\tif(k < i) {",
    "\t\t\t\t\t\t\tsmallerMatrix[((n - 1) * (j - 1)) + k] = MatrixFlat[(n * j) + k]; //MatrixFlat[n * row + column]",
    "\t\t\t\t\t\t} else if(k > i) {",
    "\t\t\t\t\t\t\tsmallerMatrix[((n - 1) * (j - 1)) + (k - 1)] = MatrixFlat[(n * j) + k];",
    "\t\t\t\t\t\t}",
    "\t\t\t\t\t}",
    "\t\t\t\t}\n",
                
    "\t\t\t\ts = (i%2 == 0) ? 1 : -1;",
    "\t\t\t\t" ++ className ++ " temporaryMatrix = new " ++ className ++ "(smallerMatrix, 1);",
	"\t\t\t\tdet += s * Matrix" ++ v2 ++ "[i] * (temporaryMatrix.determinant());",
    "\t\t\t}",
    "\t\t\treturn det;",
    "\t\t}",
    "\t}"
	]

makeDeterminantMethod (M Array a d) (MS className [r,c,b1,b2,i,j,k,j2,v2] e constructor det) =
	[
	"\tpublic long determinant() throws NotSquareMatrixException {",
	"\t\tlong det = 0, s;"
	] ++
	(arrayCopyHelper (M Array a d) (MS className [r,c,b1,b2,i,j,k,j2,v2] e constructor det)) ++
	[
	"\t\tif(n == 1) {",
    "\t\t\treturn(Matrix[0][0]);",
    "\t\t} else {",
    "\t\t\tfor(int i = 0; i < n; i++) {",
    "\t\t\t\tlong[] smallerMatrix = new long[(n - 1) * (n - 1)];",
    "\t\t\t\tfor(int " ++ j2 ++ " = " ++ b1 ++ " ; " ++ j2 ++ " < n; " ++ j2 ++ "++) {",
    "\t\t\t\t\tfor(int " ++ k ++ " = " ++ b2 ++ "; " ++ k ++ " < n; " ++ k ++ "++) {",
    "\t\t\t\t\t\tif(k < i) {",
    "\t\t\t\t\t\t\tsmallerMatrix[((n - 1) * (j - 1)) + k] = Matrix[" ++ j2 ++ "][" ++ k ++ "]" ++ "; //MatrixFlat[n * row + column]",
    "\t\t\t\t\t\t} else if(k > i) {",
    "\t\t\t\t\t\t\tsmallerMatrix[((n - 1) * (j - 1)) + (k - 1)] = Matrix[" ++ k ++ "][" ++ j2 ++ "]" ++ ";",
    "\t\t\t\t\t\t}",
    "\t\t\t\t\t}",
    "\t\t\t\t}\n",
                
    "\t\t\t\ts = (i%2 == 0) ? 1 : -1;",
    "\t\t\t\t" ++ className ++ " temporaryMatrix = new " ++ className ++ "(smallerMatrix, 1);",
	"\t\t\t\tdet += s * Matrix" ++ v2 ++ "[i] * (temporaryMatrix.determinant());",
    "\t\t\t}",
    "\t\t\treturn det;",
    "\t\t}",
    "\t}"
	]

{-
	For Record type matrices
-}
makeDeterminantMethod m ms =
	[
	"\tpublic long determinant() {\n",

	"\t\treturn ("
	] ++
	
	(map ("\t\t\t" ++) (ppDeterminant (getDimension m) (variables m ms))) ++

	[
	"\t\t);",
	"\t}"
	]

variables :: Matrix -> JavaMatrix -> [String]
variables (M _ Flat d) (MS _ [m,a,b1,b2,cl,v,v1,j2,v2] _ _ _) = 
	(map (m ++ )
		(zipWith (++)
			(foldl (++) [] $ map (replicate d) $  map show [1..d])
			(cycle $ map show [1..d])
		)
	)
variables (M _ _ d) (MS _ [m,a,b1,b2,cl,v,v1,j2,v2] _ _ _) =
		(zipWith (++)	
			(foldr (++) [] $ map (replicate d) $ map (m ++) $ map show [1..d])
			(foldr (++) [] $ replicate d $ map ((b1 ++ a) ++) $ map show [1..d])
		)

ppDeterminant :: Int -> [String] -> [String]
ppDeterminant d v = 
	zipWith (++)
		([""] ++ (cycle ["-","+"]))
		(ddH d 0 v)

ddH :: Int -> Int -> [String] -> [String]
ddH d c v = if c == d then []
	else (determinantHelper d c v) : (ddH d (c + 1) v)
	
determinantHelper :: Int -> Int -> [String] -> String
determinantHelper 1 c v = head v
determinantHelper d c (vs) = " (" ++ (vs !! c) ++ " * " ++ (unwords $ (ppDeterminant (d-1) (drop (d-1) (filterColumn d c 0 vs)))) ++ ")"


putInList :: String -> [String]
putInList s = [s]

filterColumn :: Int -> Int -> Int -> [String] -> [String]
filterColumn d c counter [] = []
filterColumn d c counter (v:vs) = if counter `mod` d /= c then (v : (filterColumn d c (counter + 1) vs))
	else filterColumn d c (counter + 1) vs


arrayCopyHelper :: Matrix -> JavaMatrix -> [String]
arrayCopyHelper (M Array Flat _) _ =
	[
	"\t\tint n = (int)(Math.sqrt(MatrixFlat.length));"
	]

arrayCopyHelper (M Array _ _) (MS className [r,c,b1,b2,i,j,k,j2,v2] e constructor det) = 
	[
    "\t\tint n = (int)(" ++ r ++ ".length);",
	"\t\tlong [][] Matrix = new long [n][n];\n",
        
    "\t\tfor(int i = 0; i < n; i++) {",
    "\t\t\tSystem.arraycopy(" ++ r ++ "[i], 0, Matrix[i], 0, n);",
    "\t\t}"
    ]

