package edu.pjwstk.api.user.dto;

import java.time.LocalDate;

public record RegisterUserApiDto(
        String firstName,
        String lastName,
        String email,
        String password,
        String username,
        LocalDate dateOfBirth,
        boolean sendBudgetReports,
        boolean isProfilePublic,
        boolean isEmailVerified
) {
}
