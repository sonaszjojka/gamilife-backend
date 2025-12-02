package pl.gamilife.auth.dto;

public record GoogleUserDto(
        String sub,
        String email,
        String firstName,
        String lastName
) {
}
