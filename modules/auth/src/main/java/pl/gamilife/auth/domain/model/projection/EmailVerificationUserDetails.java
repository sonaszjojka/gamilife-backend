package pl.gamilife.auth.domain.model.projection;

public record EmailVerificationUserDetails(
        String email,
        boolean verified
) {
}
