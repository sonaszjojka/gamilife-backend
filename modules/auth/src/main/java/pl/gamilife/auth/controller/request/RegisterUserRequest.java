package pl.gamilife.auth.controller.request;

import jakarta.validation.constraints.*;

import java.time.LocalDate;

public record RegisterUserRequest(
        @Size(min = 3, max = 50, message = "First name must be between 3 and 50 characters")
        @NotBlank(message = "First name is required")
        String firstName,

        @Size(min = 3, max = 50, message = "Last name must be between 3 and 50 characters")
        @NotBlank(message = "Last name is required")
        String lastName,

        @Email(message = "Email should be valid")
        @NotBlank(message = "Email is required")
        String email,

        @Size(min = 8, max = 128, message = "Password must be between 8 and 128 characters")
        @NotBlank(message = "Password is required")
        String password,

        @Size(min = 3, max = 20, message = "Username must be between 3 and 20 characters")
        @NotBlank(message = "Username is required")
        String username,

        @NotNull(message = "Date of birth is required")
        @Past(message = "Date of birth must be in the past")
        LocalDate dateOfBirth,

        @NotNull(message = "Send budget reports preference is required")
        boolean sendBudgetReports,

        @NotNull(message = "Is profile public preference is required")
        boolean isProfilePublic
) {
}
