package jcowgill.prac17;

import java.io.BufferedReader;
import java.io.FileReader;

public class TriangleSolver
{
    public static void main(String[] args) throws Exception
    {
        // Read triangle.txt
        BufferedReader reader = new BufferedReader(new FileReader("triangle.txt"));

        try
        {
            int[][] values = new int[100][];

            for (int i = 0; i < 100; i++)
            {
                String[] numbers = reader.readLine().split(" ");
                values[i] = new int[numbers.length];

                for (int j = 0; j < numbers.length; j++)
                    values[i][j] = Integer.valueOf(numbers[j]);
            }

            // Process from second last row
            for (int i = 98; i >= 0; i--)
            {
                for (int j = 0; j < values[i].length; j++)
                {
                    // Max of lower values + my value
                    values[i][j] += Math.max(values[i + 1][j], values[i + 1][j + 1]);
                }
            }

            // Print result
            System.out.println(values[0][0]);
        }
        finally
        {
            reader.close();
        }
    }
}
