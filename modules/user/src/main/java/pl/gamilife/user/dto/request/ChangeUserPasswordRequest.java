package pl.gamilife.user.dto.request;

public record ChangeUserPasswordRequest(
        String oldPassword,
        String newPassword
) {
}
