package edu.pjwstk.common.userApi.dto;

public record CheckIfUsersEmailIsVerifiedApiDto(boolean isVerified, String email) {
}
