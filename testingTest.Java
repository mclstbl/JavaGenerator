/*
From assignment3 directory:
javac -cp .:/usr/share/java/junit.jar testingTest.java Matrix6x6RC.java Matrix6x6CR.java Matrix6x6Flat.java MatrixArrayRC.java MatrixArrayCR.java MatrixArrayFlat.java NotSquareMatrixException.java 

From JavaGenerator directory:
java -cp .:/usr/share/java/junit.jar org.junit.runner.JUnitCore assignment3.testingTest
*/

package assignment3;

import static org.junit.Assert.*;

import org.junit.Test;

public class testingTest {

    Matrix6x6Flat MatrixOne;
    Matrix6x6RC MatrixTwo;
    Matrix6x6CR MatrixThree;
    MatrixArrayFlat MatrixFour;
    MatrixArrayRC MatrixFive;
    MatrixArrayCR MatrixSix;
        
    long[] Matrix = {
            1,1,1,1,1,1,
            1,1,1,1,1,1,
            1,1,1,1,1,1,
            1,1,1,1,1,1,
            1,1,1,1,1,1,
            1,1,1,1,1,1
            };

    long expected = 0;
    
    @Test
    public void test() throws NotSquareMatrixException {

        MatrixOne = new Matrix6x6Flat(Matrix,1);
        MatrixTwo = new Matrix6x6RC(Matrix,1);
        MatrixThree = new Matrix6x6CR(Matrix,1);
        MatrixFour = new MatrixArrayFlat(Matrix,1);
        MatrixFive = new MatrixArrayRC(Matrix,1);
        MatrixSix = new MatrixArrayCR(Matrix,1);
            
        assertEquals("MatrixOne.det() must be " +expected, expected, MatrixOne.determinant());            
        assertEquals("MatrixTwo.det() must be " +expected, expected, MatrixTwo.determinant());
        assertEquals("MatrixThree.det() must be " +expected, expected, MatrixThree.determinant());
        assertEquals("MatrixFour.det() must be " +expected, expected, MatrixFour.determinant());
        assertEquals("MatrixFive.det() must be " +expected, expected, MatrixFive.determinant());            
        assertEquals("MatrixSix.det() must be " +expected, expected, MatrixSix.determinant());
        /*
        MatrixOne = new Matrix6x6Flat(Matrix,1);
        MatrixTwo = new Matrix6x6RC(Matrix,1);
        MatrixThree = new Matrix6x6CR(Matrix,1);
        MatrixFour = new MatrixArrayFlat(Matrix,1);
        MatrixFive = new MatrixArrayRC(Matrix,1);
        MatrixSix = new MatrixArrayCR(Matrix,1);
        */   
        assertEquals("MatrixOne.det() must equal MatrixOne.det()", MatrixOne.determinant(), MatrixOne.determinant());
        assertEquals("MatrixOne.det() must equal MatrixTwo.det()", MatrixOne.determinant(), MatrixTwo.determinant());
        assertEquals("MatrixOne.det() must equal MatrixThree.det()", MatrixOne.determinant(), MatrixThree.determinant());
        assertEquals("MatrixOne.det() must equal MatrixFour.det()", MatrixOne.determinant(), MatrixFour.determinant());
        assertEquals("MatrixOne.det() must equal MatrixFive.det()", MatrixOne.determinant(), MatrixFive.determinant());
        assertEquals("MatrixOne.det() must equal MatrixSix.det()", MatrixOne.determinant(), MatrixSix.determinant());
            
        assertEquals("MatrixTwo.det() must equal MatrixOne.det()", MatrixTwo.determinant(), MatrixOne.determinant());
        assertEquals("MatrixTwo.det() must equal MatrixTwo.det()", MatrixTwo.determinant(), MatrixTwo.determinant());
        assertEquals("MatrixTwo.det() must equal MatrixThree.det()", MatrixTwo.determinant(), MatrixThree.determinant());
        assertEquals("MatrixTwo.det() must equal MatrixFour.det()", MatrixTwo.determinant(), MatrixFour.determinant());
        assertEquals("MatrixTwo.det() must equal MatrixFive.det()", MatrixTwo.determinant(), MatrixFive.determinant());
        assertEquals("MatrixTwo.det() must equal MatrixSix.det()", MatrixTwo.determinant(), MatrixSix.determinant());
            
        assertEquals("MatrixThree.det() must equal MatrixOne.det()", MatrixThree.determinant(), MatrixOne.determinant());
        assertEquals("MatrixThree.det() must equal MatrixTwo.det()", MatrixThree.determinant(), MatrixTwo.determinant());
        assertEquals("MatrixThree.det() must equal MatrixThree.det()", MatrixThree.determinant(), MatrixThree.determinant());
        assertEquals("MatrixThree.det() must equal MatrixFour.det()", MatrixThree.determinant(), MatrixFour.determinant());
        assertEquals("MatrixThree.det() must equal MatrixFive.det()", MatrixThree.determinant(), MatrixFive.determinant());
        assertEquals("MatrixThree.det() must equal MatrixSix.det()", MatrixThree.determinant(), MatrixSix.determinant());
            
        assertEquals("MatrixFour.det() must equal MatrixOne.det()", MatrixFour.determinant(), MatrixOne.determinant());
        assertEquals("MatrixFour.det() must equal MatrixTwo.det()", MatrixFour.determinant(), MatrixTwo.determinant());
        assertEquals("MatrixFour.det() must equal MatrixThree.det()", MatrixFour.determinant(), MatrixThree.determinant());
        assertEquals("MatrixFour.det() must equal MatrixFour.det()", MatrixFour.determinant(), MatrixFour.determinant());
        assertEquals("MatrixFour.det() must equal MatrixFive.det()", MatrixFour.determinant(), MatrixFive.determinant());
        assertEquals("MatrixFour.det() must equal MatrixSix.det()", MatrixFour.determinant(), MatrixSix.determinant());
            
        assertEquals("MatrixFive.det() must equal MatrixOne.det()", MatrixFive.determinant(), MatrixOne.determinant());
        assertEquals("MatrixFive.det() must equal MatrixTwo.det()", MatrixFive.determinant(), MatrixTwo.determinant());
        assertEquals("MatrixFive.det() must equal MatrixThree.det()", MatrixFive.determinant(), MatrixThree.determinant());
        assertEquals("MatrixFive.det() must equal MatrixFour.det()", MatrixFive.determinant(), MatrixFour.determinant());
        assertEquals("MatrixFive.det() must equal MatrixFive.det()", MatrixFive.determinant(), MatrixFive.determinant());
        assertEquals("MatrixFive.det() must equal MatrixSix.det()", MatrixFive.determinant(), MatrixSix.determinant());
            
        assertEquals("MatrixSix.det() must equal MatrixOne.det()", MatrixSix.determinant(), MatrixOne.determinant());
        assertEquals("MatrixSix.det() must equal MatrixTwo.det()", MatrixSix.determinant(), MatrixTwo.determinant());
        assertEquals("MatrixSix.det() must equal MatrixThree.det()", MatrixSix.determinant(), MatrixThree.determinant());
        assertEquals("MatrixSix.det() must equal MatrixFour.det()", MatrixSix.determinant(), MatrixFour.determinant());
        assertEquals("MatrixSix.det() must equal MatrixFive.det()", MatrixSix.determinant(), MatrixFive.determinant());
        assertEquals("MatrixSix.det() must equal MatrixSix.det()", MatrixSix.determinant(), MatrixSix.determinant());
    }
}
