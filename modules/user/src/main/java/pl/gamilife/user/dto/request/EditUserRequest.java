package edu.pjwstk.user.dto.request;

import jakarta.validation.constraints.*;

import java.time.LocalDate;

public record EditUserRequest(
        @NotBlank(message = "First name cannot be blank")
        @Size(max = 50, message = "First name cannot exceed 50 characters")
        String firstName,

        @NotBlank(message = "Last name cannot be blank")
        @Size(max = 50, message = "Last name cannot exceed 50 characters")
        String lastName,

        @NotBlank(message = "Username cannot be blank")
        @Size(max = 30, message = "Username cannot exceed 30 characters")
        String username,

        @NotNull(message = "Date of birth cannot be null")
        @Past(message = "Date of birth must be in the past")
        LocalDate dateOfBirth,

        @NotNull(message = "Send budget reports preference cannot be null")
        Boolean sendBudgetReports,

        @NotNull(message = "Profile public preference cannot be null")
        Boolean isProfilePublic
) {
}
