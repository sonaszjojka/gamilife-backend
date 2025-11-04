package edu.pjwstk.auth.usecase.command;

import edu.pjwstk.auth.validators.SecurePassword;
import edu.pjwstk.core.Command;

import java.io.Serializable;
import java.time.LocalDate;

public record RegisterUserCommand(
        String firstName,
        String lastName,
        String email,
        @SecurePassword
        String password,
        String username,
        LocalDate dateOfBirth,
        boolean sendBudgetReports,
        boolean isProfilePublic
) implements Serializable, Command {
}
