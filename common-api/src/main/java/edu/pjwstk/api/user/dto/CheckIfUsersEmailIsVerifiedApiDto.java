package edu.pjwstk.api.user.dto;

public record CheckIfUsersEmailIsVerifiedApiDto(boolean isVerified, String email) {
}
