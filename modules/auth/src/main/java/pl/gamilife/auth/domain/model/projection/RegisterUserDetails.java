package pl.gamilife.auth.domain.model.projection;

import java.time.LocalDate;

public record RegisterUserDetails(
        String firstName,
        String lastName,
        String email,
        String password,
        String username,
        LocalDate dateOfBirth,
        boolean sendBudgetReports,
        boolean isProfilePublic,
        boolean isEmailVerified,
        boolean isTutorialCompleted
) {
}
