package pl.gamilife.user.dto.service;

import java.time.LocalDate;
import java.util.UUID;

public record UserDetails(
        UUID id,
        String firstName,
        String lastName,
        String email,
        String username,
        LocalDate dateOfBirth,
        int experience,
        int level,
        int money,
        boolean sendBudgetReports,
        boolean isProfilePublic,
        boolean isEmailVerified,
        boolean isTutorialCompleted
) {
}
