#include <stdio.h>
#include <stdlib.h>
#include <complex.h>
#include <math.h>
#include <string.h>
#include <ctype.h>

#define TOLERANCE 1e-12
#define I _Complex_I

// Function to get a complex number input from the user
double complex get_complex_input(const char *prompt) {
    char input[100];
    double real = 0.0, imag = 0.0;

    while (1) {
        printf("%s", prompt);

        if (!fgets(input, sizeof(input), stdin)) {
            fprintf(stderr, "Error reading input.\n");
            exit(1);
        }

        input[strcspn(input, "\n")] = '\0';

        // Handle empty input
        if (strlen(input) == 0) {
            printf("Input cannot be empty. Please enter a complex number.\n");
            continue;
        }

        // Remove spaces and convert 'i' to 'j'
        char temp[100];
        int idx = 0;
        for (int i = 0; input[i] != '\0'; i++) {
            if (!isspace((unsigned char)input[i])) {
                temp[idx++] = (input[i] == 'i') ? 'j' : input[i];
            }
        }
        temp[idx] = '\0';

        // Special case for just 'j' or '-j'
        if (strcmp(temp, "j") == 0) {
            return I;
        } else if (strcmp(temp, "-j") == 0) {
            return -I;
        }

        char *endptr;
        if (temp[strlen(temp) - 1] == 'j') {
            temp[strlen(temp) - 1] = '\0';
            
            int sign_pos = -1;
            for (int i = strlen(temp) - 1; i >= 1; i--) {
                if (temp[i] == '+' || temp[i] == '-') {
                    sign_pos = i;
                    break;
                }
            }

            if (sign_pos != -1) {
                char real_part[50], imag_part[50];
                strncpy(real_part, temp, sign_pos);
                real_part[sign_pos] = '\0';
                strcpy(imag_part, &temp[sign_pos]);

                if (strlen(imag_part) == 1) { // Just '+' or '-'
                    imag = (imag_part[0] == '+') ? 1.0 : -1.0;
                } else {
                    imag = strtod(imag_part, &endptr);
                }
                real = strtod(real_part, &endptr);
            } else {
                if (strlen(temp) == 0) {
                    imag = 1.0;
                } else {
                    imag = strtod(temp, &endptr);
                }
                real = 0.0;
            }
        } else {
            real = strtod(temp, &endptr);
            imag = 0.0;
        }

        return real + imag * I;
    }
}

// Function to format a complex number in Cartesian form (a + bj)
void format_complex(double complex num, char *buffer, double tolerance) {
    double real = creal(num);
    double imag = cimag(num);

    if (fabs(real) < tolerance) real = 0.0;
    if (fabs(imag) < tolerance) imag = 0.0;

    if (fabs(real) < tolerance && fabs(imag) < tolerance) {
        sprintf(buffer, "0");
    } else if (fabs(real) < tolerance) {
        if (fabs(imag - 1.0) < tolerance) {
            sprintf(buffer, "j");
        } else if (fabs(imag + 1.0) < tolerance) {
            sprintf(buffer, "-j");
        } else {
            sprintf(buffer, "%.2fj", imag);
        }
    } else if (fabs(imag) < tolerance) {
        sprintf(buffer, "%.2f", real);
    } else {
        if (imag > 0) {
            if (fabs(imag - 1.0) < tolerance) {
                sprintf(buffer, "%.2f+j", real);
            } else {
                sprintf(buffer, "%.2f+%.2fj", real, imag);
            }
        } else {
            if (fabs(imag + 1.0) < tolerance) {
                sprintf(buffer, "%.2f-j", real);
            } else {
                sprintf(buffer, "%.2f%.2fj", real, imag);
            }
        }
    }
}

// Function to format a complex number in polar form
void format_polar(double complex num, char *buffer) {
    double magnitude = cabs(num);
    double angle_deg = carg(num) * 180.0 / M_PI;

    if (magnitude < TOLERANCE) {
        sprintf(buffer, "0");
    } else {
        if (angle_deg < 0) angle_deg += 360.0;
        sprintf(buffer, "%.2f∠%.1f°", magnitude, angle_deg);
    }
}

// Function to format a complex number as a sinusoidal function
void format_sinusoidal(double complex num, char *buffer) {
    double magnitude = cabs(num);
    double angle_deg = carg(num) * 180.0 / M_PI;

    if (magnitude < TOLERANCE) {
        sprintf(buffer, "0");
    } else {
        if (angle_deg < 0) angle_deg += 360.0;

        if (fabs(angle_deg) < 0.1 || fabs(angle_deg - 360.0) < 0.1) {
            sprintf(buffer, "%.2fcos(t)", magnitude);
        } else if (angle_deg <= 180.0) {
            sprintf(buffer, "%.2fcos(t + %.1f°)", magnitude, angle_deg);
        } else {
            sprintf(buffer, "%.2fcos(t - %.1f°)", magnitude, 360.0 - angle_deg);
        }
    }
}

// Function to solve complex system using Gaussian Elimination with Partial Pivoting
int solve_complex_system(int n, double complex **A, double complex *b, double complex *x) {
    // Create augmented matrix
    double complex **aug = malloc(n * sizeof(double complex *));
    if (!aug) {
        fprintf(stderr, "Memory allocation failed\n");
        return -1;
    }
    for (int i = 0; i < n; i++) {
        aug[i] = malloc((n + 1) * sizeof(double complex));
        if (!aug[i]) {
            fprintf(stderr, "Memory allocation failed\n");
            // Free previously allocated rows
            for (int k = 0; k < i; k++) {
                free(aug[k]);
            }
            free(aug);
            return -1;
        }
        for (int j = 0; j < n; j++) {
            aug[i][j] = A[i][j];
        }
        aug[i][n] = b[i];
    }

    // Forward elimination
    for (int i = 0; i < n; i++) {
        // Partial pivoting
        int max_row = i;
        double max_val = cabs(aug[i][i]);
        for (int k = i + 1; k < n; k++) {
            double current_val = cabs(aug[k][i]);
            if (current_val > max_val) {
                max_val = current_val;
                max_row = k;
            }
        }

        if (max_val < TOLERANCE) {
            fprintf(stderr, "Matrix is singular or nearly singular\n");
            // Free augmented matrix
            for (int k = 0; k < n; k++) {
                free(aug[k]);
            }
            free(aug);
            return -1;
        }

        // Swap rows if needed
        if (max_row != i) {
            double complex *temp_row = aug[i];
            aug[i] = aug[max_row];
            aug[max_row] = temp_row;
        }

        // Eliminate entries below pivot
        for (int k = i + 1; k < n; k++) {
            double complex factor = aug[k][i] / aug[i][i];
            for (int j = i; j <= n; j++) {
                aug[k][j] -= factor * aug[i][j];
            }
        }
    }

    // Back substitution
    for (int i = n - 1; i >= 0; i--) {
        if (cabs(aug[i][i]) < TOLERANCE) {
            fprintf(stderr, "Matrix is singular or nearly singular\n");
            // Free augmented matrix
            for (int k = 0; k < n; k++) {
                free(aug[k]);
            }
            free(aug);
            return -1;
        }

        x[i] = aug[i][n];
        for (int j = i + 1; j < n; j++) {
            x[i] -= aug[i][j] * x[j];
        }
        x[i] /= aug[i][i];
    }

    // Free augmented matrix
    for (int i = 0; i < n; i++) {
        free(aug[i]);
    }
    free(aug);

    return 0;
}

// Function to display the matrix A and vector b
void display_matrix(int n, double complex **A, double complex *b) {
    char buffer[100];
    printf("\nMatrix A and vector b:\n");
    for (int i = 0; i < n; i++) {
        printf("| ");
        for (int j = 0; j < n; j++) {
            format_complex(A[i][j], buffer, TOLERANCE);
            printf("%-15s ", buffer);
        }
        printf("|   | v%d |   =   ", i + 1);
        format_complex(b[i], buffer, TOLERANCE);
        printf("%-15s\n", buffer);
    }
}

int main() {
    printf("\nComplex-Valued Matrix Equation Solver for Steady-State Analysis\n");
    printf("This solver accepts complex numbers in both matrix A and vector b\n");
    printf("Use 'j' for the imaginary unit (e.g., 1+2j, 3j, 4-5j)\n\n");

    int n;
    while (1) {
        printf("Enter the number of unknowns (n, between 2 and 4): ");
        if (scanf("%d", &n) != 1) {
            printf("Invalid input. Please enter a positive integer.\n");
            while (getchar() != '\n');
            continue;
        }
        if (n < 2 || n > 4) {
            printf("Number of unknowns must be between 2 and 4.\n");
            continue;
        }
        while (getchar() != '\n');
        break;
    }

    // Allocate memory
    double complex **A = malloc(n * sizeof(double complex *));
    if (!A) {
        fprintf(stderr, "Memory allocation failed\n");
        return 1;
    }
    for (int i = 0; i < n; i++) {
        A[i] = malloc(n * sizeof(double complex));
        if (!A[i]) {
            fprintf(stderr, "Memory allocation failed\n");
            // Free previously allocated rows
            for (int k = 0; k < i; k++) {
                free(A[k]);
            }
            free(A);
            return 1;
        }
    }
    
    double complex *b = malloc(n * sizeof(double complex));
    double complex *x = malloc(n * sizeof(double complex));
    if (!b || !x) {
        fprintf(stderr, "Memory allocation failed\n");
        // Free allocated memory
        for (int i = 0; i < n; i++) {
            free(A[i]);
        }
        free(A);
        if (b) free(b);
        if (x) free(x);
        return 1;
    }

    // Get matrix A
    printf("\nEnter the elements of matrix A (complex numbers allowed):\n");
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            char prompt[50];
            sprintf(prompt, "A[%d][%d] = ", i + 1, j + 1);
            A[i][j] = get_complex_input(prompt);
        }
    }

    // Get vector b
    printf("\nEnter the elements of vector b (complex numbers allowed):\n");
    for (int i = 0; i < n; i++) {
        char prompt[50];
        sprintf(prompt, "b[%d] = ", i + 1);
        b[i] = get_complex_input(prompt);
    }

    // Display the system
    display_matrix(n, A, b);

    // Solve the system
    if (solve_complex_system(n, A, b, x) == 0) {
        // Display results in Cartesian form
        printf("\nSolution vector x in Cartesian form:\n");
        for (int i = 0; i < n; i++) {
            char buffer[100];
            format_complex(x[i], buffer, TOLERANCE);
            printf("v%d = %s\n", i + 1, buffer);
        }

        // Display results in polar form
        printf("\nSolution vector x in polar form:\n");
        for (int i = 0; i < n; i++) {
            char buffer[100];
            format_polar(x[i], buffer);
            printf("v%d = %s\n", i + 1, buffer);
        }

        // Display steady-state responses
        printf("\nSteady-State Response Values:\n");
        for (int i = 0; i < n; i++) {
            char buffer[100];
            format_sinusoidal(x[i], buffer);
            printf("v%d(t) = %s\n", i + 1, buffer);
        }
    }

    // Clean up
    for (int i = 0; i < n; i++) {
        free(A[i]);
    }
    free(A);
    free(b);
    free(x);

    return 0;
}
