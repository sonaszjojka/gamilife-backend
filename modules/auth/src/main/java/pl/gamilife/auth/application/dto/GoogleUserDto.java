package pl.gamilife.auth.application.dto;

public record GoogleUserDto(
        String sub,
        String email,
        String firstName,
        String lastName
) {
}
