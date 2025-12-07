package edu.pjwstk.user.dto.service;

import edu.pjwstk.core.Command;

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