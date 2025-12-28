package pl.gamilife.auth.domain.port.context;

import pl.gamilife.auth.domain.model.projection.BasicUserDetails;
import pl.gamilife.auth.domain.model.projection.EmailVerificationUserDetails;
import pl.gamilife.auth.domain.model.projection.RegisterUserDetails;
import pl.gamilife.auth.domain.model.projection.SecureUserDetails;

import java.util.Optional;
import java.util.UUID;

public interface UserContext {
    Optional<SecureUserDetails> getSecureUserDataById(UUID userId);

    Optional<SecureUserDetails> getSecureUserDataByEmail(String email);

    BasicUserDetails confirmUserEmailVerification(UUID uuid);

    Optional<BasicUserDetails> getUserByEmail(String email);

    BasicUserDetails registerNewUser(RegisterUserDetails user);

    EmailVerificationUserDetails checkIfUsersEmailIsVerified(UUID userId);

    void updateUserEmail(UUID userId, String newEmail);

    void updateUserPassword(UUID userId, String encodedPassword);
}
