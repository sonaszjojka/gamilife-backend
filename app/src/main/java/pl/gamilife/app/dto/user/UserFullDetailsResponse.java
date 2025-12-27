package pl.gamilife.app.dto.user;

import java.time.LocalDate;
import java.util.UUID;

public record UserFullDetailsResponse(
        UUID id,
        String firstName,
        String lastName,
        String email,
        String username,
        LocalDate dateOfBirth,
        int experience,
        Integer level,
        Integer requiredExperience,
        int money,
        boolean sendBudgetReports,
        boolean isProfilePublic,
        boolean isEmailVerified,
        boolean isTutorialCompleted

) implements UserDetailsResponse {
}
