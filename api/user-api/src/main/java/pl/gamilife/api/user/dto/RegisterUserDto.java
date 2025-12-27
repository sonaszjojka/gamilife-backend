package pl.gamilife.api.user.dto;

import java.time.LocalDate;
import java.time.ZoneId;

public record RegisterUserDto(
        String firstName,
        String lastName,
        String email,
        String password,
        String username,
        LocalDate dateOfBirth,
        boolean sendBudgetReports,
        boolean isProfilePublic,
        boolean isEmailVerified,
        boolean isTutorialCompleted,
        ZoneId timezone
) {
}
