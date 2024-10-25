#include <stdio.h>
#include <stdlib.h>
#include <complex.h>
#include <math.h>
#include <string.h>
#include <ctype.h>

// Tolerance for comparing floating-point numbers to zero
#define TOLERANCE 1e-12 
#define I _Complex_I  

// Function to get a complex number input from the user
double complex get_complex_input(const char *prompt) {
    char input[100];                // Buffer to store user input
    double real = 0.0, imag = 0.0;  // Variables to store real and imaginary parts

    while (1) {
        printf("%s", prompt);       // Display the prompt message

        // Read a line of input from the user
        if (!fgets(input, sizeof(input), stdin)) {
            fprintf(stderr, "Error reading input.\n");
            exit(1);
        }

        // Remove the newline character at the end of the input
        input[strcspn(input, "\n")] = '\0';

        // Remove all spaces from the input string
        char temp[100];
        int idx = 0;
        for (int i = 0; input[i] != '\0'; i++) {
            if (!isspace((unsigned char)input[i])) {
                temp[idx++] = input[i];  // Copy non-space characters to temp
            }
        }
        temp[idx] = '\0';  // Null-terminate the temp string

        // Replace 'i' with 'j' to standardize the notation for the imaginary unit
        for (int i = 0; temp[i] != '\0'; i++) {
            if (temp[i] == 'i') temp[i] = 'j';
        }

        // Handle empty input by prompting the user again
        if (strlen(temp) == 0) {
            printf("Input cannot be empty. Please enter a complex number.\n");
            continue;
        }

        // Pointer for strtod function to check if conversion is successful
        char *endptr;

        // Check if the input ends with 'j', indicating an imaginary number
        if (temp[strlen(temp) - 1] == 'j') {
            // Remove 'j' from the end of the string
            temp[strlen(temp) - 1] = '\0';

            // Find the position of the last '+' or '-' character (excluding the first character)
            int sign_pos = -1;
            for (int i = strlen(temp) - 1; i >= 1; i--) {  // Start from the end towards the first character
                if (temp[i] == '+' || temp[i] == '-') {
                    sign_pos = i;
                    break;
                }
            }

            if (sign_pos != -1) {
                // Split the string into real_part and imag_part
                char real_part[50], imag_part[50];
                strncpy(real_part, temp, sign_pos);   // Copy real part
                real_part[sign_pos] = '\0';           // Null-terminate real_part
                strcpy(imag_part, &temp[sign_pos]);    // Copy imaginary part

                // Handle cases where imag_part is just '+' or '-'
                if (strcmp(imag_part, "+") == 0) {
                    imag = 1.0;
                } else if (strcmp(imag_part, "-") == 0) {
                    imag = -1.0;
                } else {
                    // Convert imag_part to double
                    imag = strtod(imag_part, &endptr);
                    if (*endptr != '\0') {
                        printf("Invalid input. Please enter a valid complex number using 'j' for the imaginary part.\n");
                        continue;
                    }
                }

                // Convert real_part to double
                real = strtod(real_part, &endptr);
                if (*endptr != '\0') {
                    printf("Invalid input. Please enter a valid complex number using 'j' for the imaginary part.\n");
                    continue;
                }
            } else {
                // Input is purely imaginary (e.g., "5j" or "-3j")
                if (strcmp(temp, "+") == 0) {
                    imag = 1.0;
                } else if (strcmp(temp, "-") == 0) {
                    imag = -1.0;
                } else {
                    // Convert the imaginary part to double
                    imag = strtod(temp, &endptr);
                    if (*endptr != '\0') {
                        printf("Invalid input. Please enter a valid complex number using 'j' for the imaginary part.\n");
                        continue;
                    }
                }
                real = 0.0;  // Real part is zero
            }
        } else {
            // Input is purely real (e.g., "5" or "-3")
            real = strtod(temp, &endptr);
            if (*endptr != '\0') {
                printf("Invalid input. Please enter a valid complex number using 'j' for the imaginary part.\n");
                continue;
            }
            imag = 0.0;  // Imaginary part is zero
        }

        // Return the complex number constructed from real and imaginary parts
        return real + imag * I;
    }
}

// Function to format a complex number in Cartesian form (a + bj)
void format_complex(double complex num, char *buffer, double tolerance) {
    double real = creal(num);  // Get the real part
    double imag = cimag(num);  // Get the imaginary part

    // Set real or imaginary parts to zero if they are within the specified tolerance
    if (fabs(real) < tolerance) real = 0.0;
    if (fabs(imag) < tolerance) imag = 0.0;

    // Format the complex number based on which parts are non-zero
    if (fabs(real) < tolerance && fabs(imag) < tolerance) {
        // Both real and imaginary parts are zero
        sprintf(buffer, "0");
    } else if (fabs(real) < tolerance) {
        // Only the imaginary part is non-zero
        sprintf(buffer, "%.2fj", imag);
    } else if (fabs(imag) < tolerance) {
        // Only the real part is non-zero
        sprintf(buffer, "%.2f", real);
    } else {
        // Both real and imaginary parts are non-zero
        if (imag >= 0)
            sprintf(buffer, "%.2f+%.2fj", real, imag);   // Imaginary part is positive
        else
            sprintf(buffer, "%.2f%.2fj", real, imag);    // Imaginary part is negative
    }
}

// Function to format a complex number in polar form (magnitude ∠ angle°)
void format_polar(double complex num, char *buffer) {
    double magnitude = cabs(num);                  // Calculate the magnitude
    double angle_deg = carg(num) * 180.0 / M_PI;   // Calculate the angle in degrees

    if (magnitude < 1e-12) {
        // Magnitude is effectively zero
        sprintf(buffer, "0");
    } else {
        if (angle_deg < 0) angle_deg += 360.0;     // Adjust negative angles
        sprintf(buffer, "%.2f∠%.1f°", magnitude, angle_deg);  // Format the output
    }
}

// Function to format a complex number as a sinusoidal function for steady-state response
void format_sinusoidal(double complex num, char *buffer) {
    double magnitude = cabs(num);                  // Calculate the magnitude
    double angle_deg = carg(num) * 180.0 / M_PI;   // Calculate the angle in degrees

    if (magnitude < 1e-12) {
        // Magnitude is effectively zero
        sprintf(buffer, "0");
    } else {
        if (angle_deg < 0) angle_deg += 360.0;     // Adjust negative angles

        // Format the sinusoidal function based on the angle
        if (fabs(angle_deg) < 0.1 || fabs(angle_deg - 360.0) < 0.1) {
            // Angle is approximately 0°
            sprintf(buffer, "%.2fcos(t)", magnitude);
        } else if (angle_deg <= 180.0) {
            // Angle between 0° and 180°
            sprintf(buffer, "%.2fcos(t + %.1f°)", magnitude, angle_deg);
        } else {
            // Angle between 180° and 360°
            sprintf(buffer, "%.2fcos(t - %.1f°)", magnitude, 360.0 - angle_deg);
        }
    }
}

// Function to solve a system of linear equations with complex coefficients using Gaussian elimination
int solve_complex_system(int n, double complex **A, double complex *b, double complex *x) {
    int i, j, k, max_row;               // Loop variables and pivot row index
    double max_val;                     // Maximum value for pivoting
    double complex tmp;                 // Temporary variable for calculations
    double complex **augmented_matrix;  // Augmented matrix [A | b]

    // Allocate memory for the augmented matrix
    augmented_matrix = malloc(n * sizeof(double complex *));
    if (augmented_matrix == NULL) {
        fprintf(stderr, "Memory allocation error.\n");
        return -1;
    }
    for (i = 0; i < n; i++) {
        augmented_matrix[i] = malloc((n + 1) * sizeof(double complex));
        if (augmented_matrix[i] == NULL) {
            fprintf(stderr, "Memory allocation error.\n");
            return -1;
        }
    }

    // Initialize the augmented matrix with coefficients and constants
    for (i = 0; i < n; i++) {
        for (j = 0; j < n; j++) {
            augmented_matrix[i][j] = A[i][j];  // Copy coefficients
        }
        augmented_matrix[i][n] = b[i];         // Append constants
    }

    // Forward elimination process
    for (k = 0; k < n; k++) {
        // Partial pivoting to improve numerical stability
        max_row = k;
        max_val = cabs(augmented_matrix[k][k]);  // Get the absolute value of the pivot element
        for (i = k + 1; i < n; i++) {
            if (cabs(augmented_matrix[i][k]) > max_val) {
                max_val = cabs(augmented_matrix[i][k]);
                max_row = i;  // Update the row with the maximum value
            }
        }

        // Check if the matrix is singular or nearly singular
        if (cabs(augmented_matrix[max_row][k]) < TOLERANCE) {
            fprintf(stderr, "\nError: Matrix is singular or nearly singular.\n");
            return -1;  // Cannot proceed further
        }

        // Swap the current row with the row having the maximum pivot element
        if (max_row != k) {
            double complex *temp_row = augmented_matrix[k];
            augmented_matrix[k] = augmented_matrix[max_row];
            augmented_matrix[max_row] = temp_row;
        }

        // Eliminate the entries below the pivot element
        for (i = k + 1; i < n; i++) {
            tmp = augmented_matrix[i][k] / augmented_matrix[k][k];  // Calculate the multiplier
            for (j = k; j < n + 1; j++) {
                augmented_matrix[i][j] -= tmp * augmented_matrix[k][j];  // Update the row
            }
        }
    }

    // Back substitution process
    for (i = n - 1; i >= 0; i--) {
        tmp = augmented_matrix[i][n];  // Start with the constant term
        for (j = i + 1; j < n; j++) {
            tmp -= augmented_matrix[i][j] * x[j];  // Subtract the known terms
        }
        x[i] = tmp / augmented_matrix[i][i];  // Solve for x[i]
    }

    // Free the allocated memory for the augmented matrix
    for (i = 0; i < n; i++) {
        free(augmented_matrix[i]);
    }
    free(augmented_matrix);

    return 0;  // Return success
}

// Function to display the matrix A and vector b
void display_matrix(int n, double complex **A, double complex *b) {
    char buffer[100];
    printf("\nMatrix A and vector b:\n");
    for (int i = 0; i < n; i++) {
        printf("| ");
        for (int j = 0; j < n; j++) {
            format_complex(A[i][j], buffer, TOLERANCE);
            printf("%10s ", buffer);
        }
        printf("|   | v%d |   =   ", i + 1);
        format_complex(b[i], buffer, TOLERANCE);
        printf("%s\n", buffer);
    }
}

int main() {
    // Introduction messages
    printf("\nComplex-Valued Matrix Equation Solver for Steady-State Analysis\n");
    printf("This solver accepts complex numbers in both matrix A and vector b\n");
    printf("Use 'j' for the imaginary unit (e.g., 1+2j, 3j, 4-5j)\n\n");

    int n;  // Variable to store the number of unknowns
    while (1) {
        printf("Enter the number of unknowns (n): ");
        if (scanf("%d", &n) != 1) {  // Read the number of unknowns
            printf("Invalid input. Please enter a positive integer.\n");
            while (getchar() != '\n');  // Clear the input buffer
            continue;
        }
        if (n <= 0) {
            printf("Number of unknowns must be positive.\n");
            continue;
        }
        while (getchar() != '\n');  // Clear the input buffer
        break;  // Valid input received
    }

    // Allocate memory for the coefficient matrix A and vectors b and x
    double complex **A = malloc(n * sizeof(double complex *));  // Coefficient matrix
    if (A == NULL) {
        fprintf(stderr, "Memory allocation error for matrix A.\n");
        return 1;
    }
    for (int i = 0; i < n; i++) {
        A[i] = malloc(n * sizeof(double complex));
        if (A[i] == NULL) {
            fprintf(stderr, "Memory allocation error for matrix A row %d.\n", i + 1);
            return 1;
        }
    }
    double complex *b = malloc(n * sizeof(double complex));  // Constants vector
    if (b == NULL) {
        fprintf(stderr, "Memory allocation error for vector b.\n");
        return 1;
    }
    double complex *x = malloc(n * sizeof(double complex));  // Solution vector
    if (x == NULL) {
        fprintf(stderr, "Memory allocation error for vector x.\n");
        return 1;
    }

    // Prompt the user to enter the elements of matrix A
    printf("\nEnter the elements of matrix A (complex numbers allowed):\n");
    for (int i = 0; i < n; i++) {  // Loop over rows
        for (int j = 0; j < n; j++) {  // Loop over columns
            char prompt[50];
            sprintf(prompt, "A[%d][%d] = ", i + 1, j + 1);  // Prepare the prompt message
            A[i][j] = get_complex_input(prompt);  // Get the complex input
        }
    }

    // Prompt the user to enter the elements of vector b
    printf("\nEnter the elements of vector b (complex numbers allowed):\n");
    for (int i = 0; i < n; i++) {
        char prompt[50];
        sprintf(prompt, "b[%d] = ", i + 1);  // Prepare the prompt message
        b[i] = get_complex_input(prompt);  // Get the complex input
    }

    // Display the matrix A and vector b before solving
    display_matrix(n, A, b);

    // Solve the system of equations
    if (solve_complex_system(n, A, b, x) != 0) {
        fprintf(stderr, "Failed to solve the system.\n");
        return 1;  // Exit with an error code
    }

    // Display the solution vector in Cartesian form
    printf("\nSolution vector x in Cartesian form:\n");
    for (int i = 0; i < n; i++) {
        char buffer[100];
        format_complex(x[i], buffer, TOLERANCE);  // Format the complex number
        printf("v%d = %s\n", i + 1, buffer);  // Display the solution
    }

    // Display the solution vector in polar form
    printf("\nSolution vector x in polar form:\n");
    for (int i = 0; i < n; i++) {
        char buffer[100];
        format_polar(x[i], buffer);  // Format the complex number in polar form
        printf("v%d = %s\n", i + 1, buffer);  // Display the solution
    }

    // Display the steady-state response values as sinusoidal functions
    printf("\nSteady-State Response Values:\n");
    for (int i = 0; i < n; i++) {
        char buffer[100];
        format_sinusoidal(x[i], buffer);  // Format the complex number as a sinusoidal function
        printf("v%d(t) = %s\n", i + 1, buffer);  // Display the steady-state response
    }

    // Free the allocated memory for matrix A and vectors b and x
    for (int i = 0; i < n; i++) {
        free(A[i]);  // Free each row of matrix A
    }
    free(A);  // Free the array of pointers
    free(b);  // Free vector b
    free(x);  // Free vector x

    return 0;  // Successful execution
}
