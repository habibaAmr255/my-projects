
import java.util.Scanner;

public class finalProject {

    public static void main(String[] args) {
        Scanner input = new Scanner(System.in);

        // Ask for the number of books the system can store
        System.out.println("Enter the number of books you want to store:");
        int numBOOKS = input.nextInt();
        input.nextLine();  // make the program continue

        // Initialize book data based on the user input size
        String[][] books = new String[numBOOKS][4];

        // Initialize book count 
        int bookCount = 0;

        System.out.println("Welcome to the Library Management System");

        // Main menu do-while loop repeated till user choose to exit
        int choice;
        do {
            System.out.println(); //print an empty line 
            System.out.println("1- ADD A BOOK");
            System.out.println("2- SEARCH FOR A BOOK BY TITLE OR ID");
            System.out.println("3- ISSUE A BOOK BY ID");
            System.out.println("4- RETURN A BOOK BY ID");
            System.out.println("5- DELETE A BOOK BY ID");
            System.out.println("6- EDIT BOOK DETAILS BY ID");
            System.out.println("7- VIEW ALL BOOKS");
            System.out.println("8- Exit");
            System.out.print("ENTER YOUR CHOICE: ");
            
            // check if user input an integer and repeated till user enter a valid input
            while (!input.hasNextInt()) {
                System.out.println("Invalid input. TRY AN INTEGER.");
                input.next(); // clear the invalid input
            }
            choice = input.nextInt();
            input.nextLine();  // allow the program to continue
            
            switch (choice) {
                case 1:
                    // Add a book
                    if (bookCount >= numBOOKS) {  //check if there is empty index or not
                        System.out.println("No space to add more books.");
                        //if the number of books entered not equal to the number of index
                    } else {
                        System.out.println("Enter the title of the book:");
                        String bookTitle = input.nextLine().trim();
                        //.trim() remove the spaces in the start and the end of your input
                        while (bookTitle.isEmpty()) {
                            //if the user entered an empty string
                            System.out.println("Title cannot be empty. TRY A VALID TITLE.");
                            bookTitle = input.nextLine().trim();
                        }

                        System.out.println("Enter the description of the book:");
                        String bookDescription = input.nextLine().trim();
                        while (bookDescription.isEmpty()) {
                            System.out.println("Description cannot be empty. TRY A VALID DESCRIPTION.");
                            bookDescription = input.nextLine().trim();
                        }

                        System.out.println("Enter the availability of the book (Available/Not Available):");
                        String availability = input.nextLine().trim();
                        //will check if user enter available or not availble eith ignore ther case
                        while (availability.isEmpty() || (!availability.equalsIgnoreCase("available") && !availability.equalsIgnoreCase("not available"))) {
                            System.out.println("Availability must be 'Available' or 'Not Available'. TRY A VALID INPUT:");
                            availability = input.nextLine().trim();
                        }

                        // put the book to the next available index in the array
                        books[bookCount] = new String[]{String.valueOf(bookCount + 1), bookTitle, bookDescription, availability};
                        bookCount++;  // Increment book count
                        System.out.println("Book added successfully!");
                    }
                    break;

                case 2:
                    // Search for a book by title or ID
                    System.out.println("Enter book title or ID to search for:");
                    String searchTerm = input.nextLine().trim();

                    boolean found = false;

                    // Search by title or ID
                    for (int i = 0; i < bookCount; i++) {
                        //compare between the entered title to the saved title in the second coloumn
                       //compare between the entered id to the saved id in the first coloumn
                        if (books[i][1].equalsIgnoreCase(searchTerm) || books[i][0].equals(searchTerm)) {
                             //printf print the line in formatted output
                            //%s is for string
                            System.out.printf("Book found: ID = %s, Title = %s, Description = %s, Availability = %s%n",
                                    books[i][0], books[i][1], books[i][2], books[i][3]);
                            found = true;
                            break;
                        }
                    }

                    if (!found) {
                        System.out.println("No book found with the given title or ID.");
                    }
                    break;

                case 3:
                    // Issue a book by ID
                    System.out.print("Enter book ID to issue: ");
                    int issueBookId = input.nextInt();
                    if (issueBookId <= 0) {
                        System.out.println("Invalid input. TRY A VALID ID.");
                        break;
                    }

                    if (issueBookId > 0 && issueBookId <= bookCount) {
                          //[issueBookId -1] will go to the row the have the entered id & coloumn of availablity
                        // then he will change the availability of the book
                        if (!books[issueBookId - 1][3].equalsIgnoreCase("Not Available")) {
                            books[issueBookId - 1][3] = "Not Available";
                            System.out.println("Book with ID " + issueBookId + " has been issued.");
                        } else {
                            System.out.println("Sorry, the book is already issued.");
                        }
                    } else {
                        System.out.println("Invalid book ID.");
                    }
                    break;

                case 4:
                    // Return a book by ID
                    System.out.print("Enter book ID to return: ");
                    int returnBookId = input.nextInt();
                    if (returnBookId <= 0) {
                        System.out.println("Invalid input. Please enter a valid positive book ID.");
                        break;
                    }
                      // will check if the book isn't available and returned it
                    if (returnBookId > 0 && returnBookId <= bookCount) {
                        if (books[returnBookId - 1][3].equalsIgnoreCase("Not Available")) {
                            books[returnBookId - 1][3] = "Available";
                            System.out.println("Book with ID " + returnBookId + " has been returned.");
                        } else {
                            System.out.println("The book was not issued.");
                        }
                    } else {
                        System.out.println("Invalid book ID.");
                    }
                    break;

                case 5:
                    // Delete a book by ID
                    System.out.print("Enter book ID to delete: ");
                    String deleteId = input.nextLine().trim();

                    boolean deleted = false;
                    for (int i = 0; i < bookCount; i++) {
                        if (books[i][0].equals(deleteId)) {
                            // will deleted the book and make a new array with the deleted book and indexes less than the first
                            String[][] newBooksArray = new String[bookCount - 1][4];
                            int index = 0;
                            for (int j = 0; j < bookCount; j++) {
                                //the remained books will added to the new array whwn wach book added the index increased 
                                if (!books[j][0].equals(deleteId)) {
                                    newBooksArray[index++] = books[j];
                                }
                            }

                            books = newBooksArray; // the old array will equal to the new array
                            bookCount--;  //decrase the number of books the program have
                            System.out.println("Book with ID " + deleteId + " has been deleted.");
                            deleted = true;
                            break;
                        }
                    }

                    if (!deleted) {
                        System.out.println("Book not found.");
                    }
                    break;

                case 6:
                    // Edit book details by ID
                    System.out.print("Enter book ID to edit: ");
                    String editId = input.nextLine().trim();

                    boolean foundToEdit = false;
                    for (int i = 0; i < bookCount; i++) {
                        if (books[i][0].equals(editId)) {
                            foundToEdit = true;
                            System.out.println("Editing details for book ID: " + editId);

                            System.out.println("Current Title: " + books[i][1]);
                            System.out.print("Enter new title : ");
                            String newTitle = input.nextLine().trim();
                            if (!newTitle.isEmpty()) {
                                books[i][1] = newTitle;
                            }

                            System.out.println("Current Description: " + books[i][2]);
                            System.out.print("Enter new description ");
                            String newDescription = input.nextLine().trim();
                            if (!newDescription.isEmpty()) {
                                books[i][2] = newDescription;
                            }

                            System.out.println("Current Availability: " + books[i][3]);
                            System.out.print("Enter new availability (Available/Not Available): ");
                            String newAvailability = input.nextLine().trim();
                            if (!newAvailability.isEmpty()) {
                                books[i][3] = newAvailability;
                            }

                            System.out.println("Book details updated successfully!");
                            break;
                        }
                    }

                    if (!foundToEdit) {
                        System.out.println("Book with ID " + editId + " not found.");
                    }
                    break;

                case 7:
                    // will print the menu in formated way the reason for use printf
                    System.out.println("Showing all books:");
                    System.out.println("Book ID\tBook Name\tDescription\t\t\t\t\t\t\t\tAvailability");
                    for (String[] book : books) {
                        System.out.printf("%-8s %-20s %-50s %-15s%n", book[0], book[1], book[2], book[3]);
                    }
                    break;

                case 8:
                    System.out.println("Exiting the system. Goodbye!");
                    break;

                default:
                    System.out.println("INVALID CHOICE TRY AGAIN");
                    break;
            }
        } while (choice != 8);
    }
}
