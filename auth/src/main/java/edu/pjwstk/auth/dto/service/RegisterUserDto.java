package edu.pjwstk.auth.dto.service;

import edu.pjwstk.auth.validators.SecurePassword;

import java.io.Serializable;
import java.time.LocalDate;

public record RegisterUserDto(
        String firstName,
        String lastName,
        String email,
        @SecurePassword
        String password,
        String username,
        LocalDate dateOfBirth,
        boolean sendBudgetReports,
        boolean isProfilePublic
) implements Serializable {
}
