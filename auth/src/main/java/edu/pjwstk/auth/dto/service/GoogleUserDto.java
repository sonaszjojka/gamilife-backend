package edu.pjwstk.auth.dto.service;

public record GoogleUserDto(
        String sub,
        String email,
        String firstName,
        String lastName
) {
}
