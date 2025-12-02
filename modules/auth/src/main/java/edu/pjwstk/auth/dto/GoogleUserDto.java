package edu.pjwstk.auth.dto;

public record GoogleUserDto(
        String sub,
        String email,
        String firstName,
        String lastName
) {
}
