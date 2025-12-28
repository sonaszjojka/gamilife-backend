package pl.gamilife.api.user.dto;

public record CheckIfUsersEmailIsVerifiedDto(boolean isVerified, String email) {
}
