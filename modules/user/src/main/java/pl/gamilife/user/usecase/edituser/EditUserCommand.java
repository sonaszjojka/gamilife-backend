package pl.gamilife.user.usecase.edituser;

import pl.gamilife.shared.kernel.architecture.Command;

import java.time.LocalDate;
import java.util.UUID;

public record EditUserCommand(
        UUID userId,
        String firstName,
        String lastName,
        String username,
        LocalDate dateOfBirth,
        Boolean sendBudgetReports,
        Boolean isProfilePublic
) implements Command {
    @Override
    public void validate() {
    }
}