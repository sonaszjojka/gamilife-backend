package pl.gamilife.user.usecase.edituser;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.time.LocalDate;
import java.util.UUID;

public record EditUserCommand(
        @NotNull
        UUID userId,
        String firstName,
        String lastName,
        String username,
        LocalDate dateOfBirth,
        Boolean sendBudgetReports,
        Boolean isProfilePublic
) implements Command {
}