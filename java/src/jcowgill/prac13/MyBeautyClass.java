package jcowgill.prac13;

import java.util.Scanner;

/**
 * Class for practical 13 (first java practical)
 * 
 * @author James
 */
public class MyBeautyClass
{
    private static double readDouble(Scanner keyboard, String prompt)
    {
        System.out.print(prompt);
        return keyboard.nextDouble();
    }

    private static String readString(Scanner keyboard, String prompt)
    {
        System.out.print(prompt);
        return keyboard.next();
    }
    
    public static void question1(Scanner keyboard)
    {
        // Practical 2 - Q1
        int num1 = keyboard.nextInt();
        int num2 = keyboard.nextInt();
        int num3 = keyboard.nextInt();
        
        int max = num1;

        if (num2 > max)
            max = num2;

        if (num3 > max)
            max = num3;
        
        System.out.println(max);
    }
    
    public static void question3(Scanner keyboard)
    {
        // Read inputs
        System.out.print("Enter speed limit: ");
        int speedLimit = keyboard.nextInt();

        System.out.print("Enter clocked speed: ");
        int speed = keyboard.nextInt();
        
        // Speeding?
        if (speed > speedLimit)
        {
            // Calculate fine
            int fine = 100 + 5 * (speed - speedLimit);
            
            // Add 200 for speeds over 90mph
            if (speed >= 90)
                fine += 200;
            
            // Print fine
            System.out.println("Speeding!");
            System.out.println(" Fined £" + fine);
        }
        else
        {
            System.out.println("Not over the speed limit");
        }
    }
    
    public static void basalCalc(Scanner keyboard)
    {
        // Read inputs
        String gender = readString(keyboard, "Enter gender (M/F):");
        double weight = readDouble(keyboard, "Enter weight (lb):");
        double height = readDouble(keyboard, "Enter height (in):");
        double age = readDouble(keyboard, "Enter age (years):");
        
        // Calculate BMR
        double bmr;
        
        if (gender.equalsIgnoreCase("F"))
            bmr = 655 + (4.3 * weight) + (4.7 * height) - (4.7 * age);
        else
            bmr = 66 + (6.3 * weight) + (12.9 * height) - (6.8 * age);
        
        // Calculate choc bars
        double chocBars = Math.ceil(bmr / 230);
        
        // Print results
        System.out.println("You need to consume " + bmr + " calories per day");
        System.out.println("You would need to eat about " + chocBars +
        				   " bars of chocolate to do this");
    }
    
    public static boolean isPalendrome(String str)
    {
        // Is it a palendrome?
        int left = 0;
        int right = str.length() - 1;
        
        while (left <= right)
        {
            // Is this char the same in both strings?
            if (str.charAt(left) != str.charAt(right))
                return false;
            
            // Advance pointers
            left++;
            right--;
        }
        
        return true;
    }
    
    public static void checkPalendrome(Scanner keyboard)
    {
        // Read sentence to check
        String sentence = readString(keyboard, "Enter a sentence:");
        
        // Is it a palendrome?
        if (isPalendrome(sentence))
            System.out.println("That is a palendrome");
        else
            System.out.println("That is not a palendrome");
    }
    
    private static final String[] MONTHS = new String[]
            {
                "January", "February", "March",
                "April", "May", "June",
                "July", "August", "September",
                "October", "November", "December",
            };
    
    public static String formatDate(int year, int month, int day)
    {
        // Get month
        String monthStr;
        
        if (month < 1 || month > 12)
            return null;
        
        monthStr = MONTHS[month - 1];
        
        // Get day suffix
        String daySuffix;
        
        if (day == 1 || day == 21 || day == 31)
            daySuffix = "st";
        else if (day == 2 || day == 22)
            daySuffix = "nd";
        else if (day == 3 || day == 23)
            daySuffix = "rd";
        else if (day >= 4 && day <= 30)
            daySuffix = "th";
        else
            return null;
        
        // Validate day
        if (month == 2)
        {
            boolean isLeapYear = (year % 4 == 0) && ((year % 100 != 0) || (year % 400 == 0));
            
            if (day > 29 || (day == 29 && !isLeapYear))
                return null;
        }
        else if (day == 31 && (month == 1 || month == 3 || month == 5 || month == 7 ||
        					   month == 8 || month == 10 || month == 10))
        {
            return null;
        }

        // Return final string
        return monthStr + " " + day + daySuffix + " " + year;
    }
    
    public static void dateTranslator(Scanner keyboard)
    {
        // Read date
        String date = readString(keyboard, "Enter date in dd/mm/yyyy format:");
        
        // Extract components and convert to integers
        String[] components = date.split("/");
        int day = Integer.valueOf(components[0]);
        int month = Integer.valueOf(components[1]);
        int year = Integer.valueOf(components[2]);
        
        // Print the result
        String formattedDate = formatDate(year, month, day);
        
        if (formattedDate == null)
            System.out.println("Invalid Date");
        else
            System.out.println(formattedDate);
    }
    
	public static void main(String[] args)
	{
        Scanner keyboard = new Scanner(System.in);
        
        try
        {
            dateTranslator(keyboard);
        }
        finally
        {
            keyboard.close();
        }
	}
}
