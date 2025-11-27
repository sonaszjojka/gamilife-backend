package edu.pjwstk.user.dto.service;

import java.time.LocalDate;
import java.util.UUID;

public record UserDetailsDto(
        UUID id,
        String firstName,
        String lastName,
        String email,
        String username,
        LocalDate dateOfBirth,
        int experience,
        int money,
        boolean sendBudgetReports,
        boolean isProfilePublic,
        boolean isEmailVerified,
        boolean isTutorialCompleted
) {
}
