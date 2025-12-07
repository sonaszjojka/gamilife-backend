package pl.gamilife.user.dto.response;

import pl.gamilife.user.domain.User;
import pl.gamilife.user.dto.service.UserDetailsDto;

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
        int level,
        Integer requiredExperience,
        int money,
        boolean sendBudgetReports,
        boolean isProfilePublic,
        boolean isEmailVerified,
        boolean isTutorialCompleted

) implements UserDetailsResponse {
    public static UserFullDetailsResponse from(UserDetailsDto dto) {
        return new UserFullDetailsResponse(
                dto.id(),
                dto.firstName(),
                dto.lastName(),
                dto.email(),
                dto.username(),
                dto.dateOfBirth(),
                dto.experience(),
                dto.level(),
                dto.requiredExperience(),
                dto.money(),
                dto.sendBudgetReports(),
                dto.isProfilePublic(),
                dto.isEmailVerified(),
                dto.isTutorialCompleted()
        );
    }

    public static UserFullDetailsResponse from(User user) {
        return new UserFullDetailsResponse(
                user.getId(),
                user.getFirstName(),
                user.getLastName(),
                user.getEmail(),
                user.getUsername(),
                user.getDateOfBirth(),
                user.getExperience(),
                user.getLevel(),
                null,
                user.getMoney(),
                user.isSendBudgetReports(),
                user.isProfilePublic(),
                user.isEmailVerified(),
                user.isTutorialCompleted()
        );
    }
}
