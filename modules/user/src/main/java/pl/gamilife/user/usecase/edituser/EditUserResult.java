package pl.gamilife.user.usecase.edituser;

import lombok.Builder;

import java.io.Serializable;
import java.time.LocalDate;
import java.util.UUID;

@Builder
public record EditUserResult(
        UUID id,
        String firstName,
        String lastName,
        String email,
        String username,
        LocalDate dateOfBirth,
        boolean sendBudgetReports,
        boolean isProfilePublic,
        boolean isEmailVerified
) implements Serializable {
}
