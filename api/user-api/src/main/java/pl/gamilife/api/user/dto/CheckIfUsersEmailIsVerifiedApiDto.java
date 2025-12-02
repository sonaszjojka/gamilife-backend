package pl.gamilife.api.user.dto;

public record CheckIfUsersEmailIsVerifiedApiDto(boolean isVerified, String email) {
}
