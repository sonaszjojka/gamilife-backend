package edu.pjwstk.auth.usecase.registeruser;

import edu.pjwstk.core.Command;

import java.io.Serializable;
import java.time.LocalDate;

public record RegisterUserCommand(
        String firstName,
        String lastName,
        String email,
        String password,
        String username,
        LocalDate dateOfBirth,
        boolean sendBudgetReports,
        boolean isProfilePublic
) implements Serializable, Command {
    @Override
    public void validate() {
        // Already validated in Request
    }
}
