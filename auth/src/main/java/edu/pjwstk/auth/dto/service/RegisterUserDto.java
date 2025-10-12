package edu.pjwstk.auth.dto.service;

import java.io.Serializable;
import java.time.LocalDate;

public record RegisterUserDto(
        String firstName,
        String lastName,
        String email,
        String password,
        String username,
        LocalDate dateOfBirth,
        boolean sendBudgetReports,
        boolean isProfilePublic
) implements Serializable {
}
