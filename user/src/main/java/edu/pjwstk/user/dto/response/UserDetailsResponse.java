package edu.pjwstk.user.dto.response;

import edu.pjwstk.user.dto.service.UserDetailsDto;

import java.time.LocalDate;
import java.util.UUID;

public record UserDetailsResponse(
        UUID id,
        String firstName,
        String lastName,
        String email,
        String username,
        LocalDate dateOfBirth,
        int experience,
        int money,
        boolean sendBudgetReports,
        boolean isProfilePublic,
        boolean isEmailVerified
) {
    public static UserDetailsResponse from(UserDetailsDto dto) {
        return new UserDetailsResponse(
                dto.id(),
                dto.firstName(),
                dto.lastName(),
                dto.email(),
                dto.username(),
                dto.dateOfBirth(),
                dto.experience(),
                dto.money(),
                dto.sendBudgetReports(),
                dto.isProfilePublic(),
                dto.isEmailVerified()
        );
    }
}
