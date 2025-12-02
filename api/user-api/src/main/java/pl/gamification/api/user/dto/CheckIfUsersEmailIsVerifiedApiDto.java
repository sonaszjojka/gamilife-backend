package pl.gamification.api.user.dto;

public record CheckIfUsersEmailIsVerifiedApiDto(boolean isVerified, String email) {
}
